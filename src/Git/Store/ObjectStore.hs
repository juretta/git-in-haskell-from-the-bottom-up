{-# LANGUAGE OverloadedStrings, RecordWildCards, DoAndIfThenElse #-}

module Git.Store.ObjectStore (
    createEmptyGitRepository
  , encodeObject
  , pathForObject
  , pathForPack
  , createGitRepositoryFromPackfile
  , updateHead
  -- ?
  , readObject
  , createRef
  , getGitDirectory
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.Zlib as Z
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Control.Applicative ((<|>))
-- FIXME -> don't use isJust/fromJust
import Data.Maybe                                           (isJust, fromJust)
import Text.Printf                                          (printf)
import Git.Pack.Packfile
import Git.Pack.Delta                                       (patch)
import Git.Common                                           (GitRepository(..), eitherToMaybe, ObjectId)
-- Tree
import Git.Store.Blob
import System.FilePath
import System.Directory
import Control.Monad                                        (unless, liftM)
import Data.Char                                            (isSpace)
import Debug.Trace


data Object = ResolvedObject {
    getObjectType   :: PackObjectType
  , getContent      :: B.ByteString
  , getSize         :: Int
  , sha1            :: String
} | UnresolvedObject {
    getObjectType   :: PackObjectType
  , deltaData       :: B.ByteString
  , getSize         :: Int
}deriving (Show, Eq, Ord)

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
        let encodedObjects = encodeObjects objs
        unresolvedObjects <- writeObjects encodedObjects
        _ <- writeDeltas repo unresolvedObjects
        putStrLn "Done"
    where   writeObjects (r@(ResolvedObject objType content size sha1):xs) = do
                let (path, name) = pathForObject getName sha1
                _ <- writeObject repo r
                writeObjects xs
            writeObjects (x@(UnresolvedObject{}):xs) = liftM (x:) (writeObjects xs)
            writeObjects []     = return []
            encodeObjects       = map encodeObject

writeDeltas :: GitRepository -> [Object] -> IO ()
writeDeltas repo (x:xs) = do
    f <- writeDelta repo x
    writeDeltas repo xs
writeDeltas _ [] = return ()

writeDelta :: GitRepository -> Object -> IO (Maybe FilePath)
writeDelta repo (UnresolvedObject ty@(OBJ_REF_DELTA baseObject) content size) = do
        base <- case toObjectId ty of
            Just sha -> readObject repo sha
            _        -> return Nothing
        if isJust base then
            case patch (getContent $ fromJust base) content of
                Right target -> do
                                let base'        = fromJust base
                                    (path, name) = pathForObject (getName repo) (sha1 base')
                                    filename     = path </> name
                                    header       = headerForBlob (objectTypeToString $ getObjectType base') target
                                    blob         = header `C.append` target
                                    obj          = ResolvedObject (getObjectType base') blob (C.length target) $ hsh blob
                                _ <- writeObject repo obj
                                return $ Just filename
                Left msg     -> return Nothing
        else return Nothing -- FIXME - base object doesn't exist yet


updateHead :: GitRepository -> Packfile -> IO ()
updateHead repo (Packfile _ _ objs) = do
    let commits = filter isCommit objs
    unless (null commits) $
        let commit = head commits
            ref = "refs/heads/master"
            in
            do
                let obj = encodeObject commit
                createRef repo ref (sha1 obj)
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

type Repository = String

-- header: "type size\0"
-- sha1 $ header ++ content
readObject :: GitRepository -> ObjectId -> IO (Maybe Object)
readObject GitRepository{..} sha = do
    let (path, name) = pathForObject getName sha
        filename     = path </> name
    exists <- trace ("readObject: " ++ filename) $ doesFileExist filename
    if exists then do
        bs <- C.readFile filename
        return $ parseBlob $ inflate bs
    else return Nothing
    where parseBlob blob = eitherToMaybe $ AC.parseOnly (blobParser sha) blob
          inflate blob = B.concat $ L.toChunks $ Z.decompress $ L.fromChunks [blob]

-- header: "type size\0"
-- sha1 $ header ++ content
blobParser :: ObjectId -> AC.Parser Object
blobParser sha1 = do
   objType <- AC.string "commit" <|> AC.string "tree" <|> AC.string "blob" <|> AC.string "tag"
   AC.char ' '
   size <- AC.takeWhile AC.isDigit
   AC.char '\0'
   blob <- AC.takeByteString
   return $ ResolvedObject (obj objType) blob (read $ C.unpack size) sha1
   where obj "commit"   = OBJ_COMMIT
         obj "tree"     = OBJ_TREE
         obj "blob"     = OBJ_BLOB
         obj "tag"      = OBJ_TAG



-- header: "type size\0"
-- sha1 $ header ++ content
writeObject :: GitRepository -> Object -> IO ()
writeObject _ (UnresolvedObject {}) = error "Can't write an UnresolvedObject"
writeObject GitRepository{..} obj = do
    let (path, name) = pathForObject getName $ sha1 obj
        filename     = path </> name
    _ <- createDirectoryIfMissing True path
    L.writeFile filename $ compress (getContent obj)
    where compress data' = Z.compress $ L.fromChunks [data'] -- FIXME should data be lazy in the first place?

hsh :: B.ByteString -> String
hsh = toHex . SHA1.hash

objectTypeToString :: PackObjectType -> B.ByteString
objectTypeToString OBJ_COMMIT = "commit"
objectTypeToString OBJ_TREE   = "tree"
objectTypeToString OBJ_BLOB   = "blob"
objectTypeToString OBJ_TAG    = "tag"

encodeObject :: PackfileObject -> Object
encodeObject obj@(PackfileObject ot@(OBJ_REF_DELTA _) size content) = UnresolvedObject ot content size
encodeObject obj@(PackfileObject ot size _) =
        ResolvedObject ot blob size (hsh blob)
    where header obj' =
            let blobType = objType obj'
            in  headerForBlob blobType $ objectData obj'
          blob                              = header obj `C.append` objectData obj
          objType (PackfileObject t _ _)    = objectTypeToString t

headerForBlob :: B.ByteString -> B.ByteString -> B.ByteString
headerForBlob objType content = objType `C.append` " " `C.append` C.pack (show $ C.length content) `C.append` "\0"

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

