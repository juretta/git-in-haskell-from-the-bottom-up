{-# LANGUAGE OverloadedStrings, RecordWildCards, DoAndIfThenElse #-}

module Git.Store.ObjectStore (
    createEmptyGitRepository
  , pathForObject
  , pathForPack
  , createGitRepositoryFromPackfile
  , updateHead
  -- ?
  , readBlob
  , createRef
  , getGitDirectory
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.Zlib as Z
import qualified Crypto.Hash.SHA1 as SHA1
-- FIXME -> don't use isJust/fromJust
import Data.Maybe                                           (isJust, fromJust)
import Text.Printf                                          (printf)
import Git.Pack.Packfile
import Git.Pack.Delta                                       (patch)
import Git.Common                                           (GitRepository(..), ObjectId)
-- Tree
import Git.Store.Blob
import System.FilePath
import System.Directory
import Control.Monad                                        (unless, liftM)
import Data.Foldable                                        (forM_)
import Debug.Trace

createGitRepositoryFromPackfile :: GitRepository -> FilePath -> IO ()
createGitRepositoryFromPackfile target packFile = do
    pack <- packRead packFile
    let repoName = getName target
        repo = GitRepository repoName
    unpackPackfile repo pack
    updateHead repo pack


-- TODO properly handle the error condition here
unpackPackfile :: GitRepository -> Packfile -> IO ()
unpackPackfile _ InvalidPackfile = error "Attempting to unpack an invalid packfile"
unpackPackfile repo@GitRepository{..} (Packfile _ _ objs) = do
        unresolvedObjects <- writeObjects objs
        forM_ unresolvedObjects writeDelta
        putStrLn "Done"
    where   writeObjects (x@(PackfileObject (OBJ_REF_DELTA _) _ _):xs) = liftM (x:) (writeObjects xs)
            writeObjects ((PackfileObject objType _ content) :xs) = do
                _ <- writeBlob repo (tt objType) content
                writeObjects xs
            writeObjects []     = return []

            tt OBJ_COMMIT       = BCommit
            tt OBJ_TREE         = BTree
            tt OBJ_BLOB         = BBlob
            tt OBJ_TAG          = BTag
            tt _                = error "Unexpected blob type"

            writeDelta (PackfileObject ty@(OBJ_REF_DELTA _) _ content) = do
                    base <- case toObjectId ty of
                        Just sha -> readBlob repo sha
                        _        -> return Nothing
                    if isJust base then
                        case patch (getBlobContent $ fromJust base) content of
                            Right target -> do
                                            let base'        = fromJust base
                                            filename <- writeBlob repo (objType base') target
                                            return $ Just filename
                            Left _       -> return Nothing
                    else return Nothing -- FIXME - base object doesn't exist yet
            writeDelta _ = error "Don't expect a resolved object here"


updateHead :: GitRepository -> Packfile -> IO ()
updateHead _ InvalidPackfile = error "Unexpected invalid packfile"
updateHead repo (Packfile _ _ objs) = do
    let commits = filter isCommit objs
    unless (null commits) $
        let commit = head commits
            ref = "refs/heads/master"
            in
            do
                let (sha1, _) = encodeBlob BCommit (objectData commit)
                createRef repo ref sha1
                createSymRef repo "HEAD" ref
    where isCommit ob = objectType ob == OBJ_COMMIT

-- ref: refs/heads/master
createSymRef :: GitRepository -> String -> String -> IO ()
createSymRef repo symName ref =
        writeFile (getGitDirectory repo </> symName) $ "ref: " ++ ref ++ "\n"


createRef :: GitRepository -> String -> String -> IO ()
createRef repo ref sha = do
    let (path, name) = splitFileName ref
        dir          = (getGitDirectory repo) </> path
    _ <- createDirectoryIfMissing True dir
    writeFile (dir </> name) (sha ++ "\n")

pathForPack :: GitRepository -> FilePath
pathForPack repo = (getGitDirectory repo) </> "objects" </> "pack"

pathForObject :: String -> String -> (FilePath, String)
pathForObject repoName sha | length sha == 40 = (repoName </> ".git" </> "objects" </> pre, rest)
    where pre  = take 2 sha
          rest = drop 2 sha
pathForObject _ _ = ("", "")

-- header: "type size\0"
-- sha1 $ header ++ content
readBlob :: GitRepository -> ObjectId -> IO (Maybe Blob)
readBlob GitRepository{..} sha = do
    let (path, name) = pathForObject getName sha
        filename     = path </> name
    exists <- trace ("readBlob: " ++ filename) $ doesFileExist filename
    if exists then do
        bs <- C.readFile filename
        return $ parseBlob sha $ inflate bs
    else return Nothing
    where inflate blob = B.concat $ L.toChunks $ Z.decompress $ L.fromChunks [blob]

-- header: "type size\0"
-- sha1 $ header ++ content
encodeBlob :: BlobType -> C.ByteString -> (ObjectId, C.ByteString)
encodeBlob blobType content = do
    let header       = headerForBlob (C.pack $ show $ blobType)
        blob         = header `C.append` content
        sha1         = hsh blob
    (sha1, blob)
    where headerForBlob objType = objType `C.append` " " `C.append` C.pack (show $ C.length content) `C.append` "\0"
          hsh = toHex . SHA1.hash

writeBlob :: GitRepository -> BlobType -> C.ByteString -> IO FilePath
writeBlob GitRepository{..} blobType content = do
    let (sha1, blob) = encodeBlob blobType content
        (path, name) = pathForObject getName sha1
        filename     = path </> name
    _ <- createDirectoryIfMissing True path
    L.writeFile filename $ compress blob
    return filename
    where compress data' = Z.compress $ L.fromChunks [data'] -- FIXME should data be lazy in the first place?





createEmptyGitRepository :: FilePath -> IO ()
createEmptyGitRepository gitDir =
        mapM_ (\dir -> createDirectoryIfMissing True (gitDir </> dir)) topLevelDirectories
        where topLevelDirectories = ["objects", "refs", "hooks", "info"]

toObjectId :: PackObjectType -> Maybe ObjectId
toObjectId (OBJ_REF_DELTA base) = Just $ toHex $ B.pack base
toObjectId _                    = Nothing

toHex :: C.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

getGitDirectory :: GitRepository -> FilePath
getGitDirectory = (</> ".git") . getName

