{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}

module Git.Remote(
    clone
  , lsRemote
  , parseRemote
  , Remote(..)
) where

import qualified Data.Attoparsec.Char8 as AC
import Data.Attoparsec.Combinator
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Control.Applicative                      ((<$>))
import Control.Monad.Reader                     (runReaderT)
import System.Directory                         (removeFile, createDirectoryIfMissing)
import System.FilePath                          ((</>), takeFileName, dropExtension)
import Network.Socket                           (withSocketsDo)
import Text.Printf
import Data.Maybe
import Data.List
import Git.Common
import Git.TcpClient
import Git.PackProtocol
import Git.Store.ObjectStore
import Git.Repository

refDiscovery :: String -> String -> String
refDiscovery host repo = pktLine $ "git-upload-pack /" ++ repo ++ "\0host="++host++"\0"

toObjId :: PacketLine -> Maybe String
toObjId (FirstLine obj _ _) = Just $ C.unpack obj
toObjId (RefLine obj _)     = Just $ C.unpack obj
toObjId _                   = Nothing


-- PKT-LINE("want" SP obj-id SP capability-list LF)
-- PKT-LINE("want" SP obj-id LF)
--
-- FIXME - filter heads/tags
createNegotiationRequest :: [String] -> [PacketLine] -> String
createNegotiationRequest capabilities = concatMap (++ "") . nub . map (pktLine . (++ "\n")) . foldl' (\acc e -> if null acc then first acc e else additional acc e) [] . wants . filter filterPeeledTags . filter filterRefs
                    where wants              = mapMaybe toObjId
                          first acc obj      = acc ++ ["want " ++ obj ++ " " ++ unwords capabilities]
                          additional acc obj = acc ++ ["want " ++ obj]
                          filterPeeledTags   = not . isSuffixOf "^{}" . C.unpack . ref
                          filterRefs line    = let r = C.unpack $ ref line
                                                   predicates = map ($ r) [isPrefixOf "refs/tags/", isPrefixOf "refs/heads/"]
                                               in any id predicates

data Remote = Remote {
    getHost         :: String
  , getPort         :: Maybe Int
  , getRepository   :: String
} deriving (Eq, Show)

-- | Parse a URL using the git protocol format.
-- E.g. git://git.apache.org:9418/foo.git
--
-- Schema:
--   * git://host.xz[:port]/path/to/repo.git/
--   * git://host.xz[:port]/~[user]/path/to/repo.git/
--
-- See the "GIT URLS" section on
-- http://www.kernel.org/pub/software/scm/git/docs/git-clone.html
parseRemote :: B.ByteString -> Maybe Remote
parseRemote = eitherToMaybe . AC.parseOnly parser
    where parser = do
            host <- "git://" AC..*> domain
            port <- option Nothing (Just <$> (":" AC..*> AC.decimal))
            slash
            repo <- AC.takeByteString
            return $ Remote (C.unpack host) port (C.unpack repo)
          domain = AC.takeTill (\x -> x == '/' || x == ':')
          slash  = AC.satisfy (== '/')

repositoryName :: Remote -> String
repositoryName = takeFileName . dropExtension . getRepository

clone :: String -> IO ()
clone url =
    case parseRemote $ C.pack url of
        Just remote -> let gitRepoName = repositoryName remote
                       in clone' (GitRepository gitRepoName) remote
        _           -> putStrLn $ "Invalid URL" ++ url

lsRemote :: String -> IO ()
lsRemote url =
    case parseRemote $ C.pack url of
        Just remote -> do
            packetLines <- lsRemote' remote
            mapM_ (\line -> printf "%s\t%s\n" (C.unpack $ objId line) (C.unpack $ ref line)) packetLines
        _           -> putStrLn $ "Invalid URL" ++ url

-- .git/objects/pack/tmp_pack_6bo2La
clone' :: GitRepository -> Remote -> IO ()
clone' repo remote@Remote{..} = do
        (refs,packFile) <- receivePack remote
        let dir = pathForPack repo
            tmpPack = dir </> "tmp_pack_incoming"
        _ <- createDirectoryIfMissing True dir
        B.writeFile tmpPack packFile
        _ <- runReaderT (createGitRepositoryFromPackfile tmpPack refs) repo
        removeFile tmpPack
        putStrLn "Checking out HEAD"
        _ <- runReaderT checkoutHead repo
        putStrLn "Finished"


lsRemote' :: Remote -> IO [PacketLine]
lsRemote' Remote{..} = withSocketsDo $
    withConnection getHost (show $ fromMaybe 9418 getPort) $ \sock -> do
        let payload = refDiscovery getHost getRepository
        send sock payload
        response <- receive sock
        send sock flushPkt -- Tell the server to disconnect
        return $ parsePacket $ L.fromChunks [response]


receivePack :: Remote -> IO ([Ref], B.ByteString)
receivePack Remote{..} = withSocketsDo $
    withConnection getHost (show $ fromMaybe 9418 getPort) $ \sock -> do
        let payload = refDiscovery getHost getRepository
        send sock payload
        putStrLn "Receiving..."
        response <- receive sock
        let pack    = parsePacket $ L.fromChunks [response]
            request = createNegotiationRequest ["multi_ack_detailed", "agent=git/1.8.1"] pack ++ flushPkt ++ "0009done\n"
        {-let request = (createNegotiationRequest ["multi_ack_detailed", "side-band-64k", "thin-pack", "ofs-delta", "agent=git/1.8.1"] pack) ++ flushPkt ++ "0009done\n"-}
        putStrLn $ "Sending ref netgotiation request"
        send sock request
        putStrLn "Receiving pack file response"
        -- FIXME - response might contain more packet lines
        !rawPack <- receiveFully sock
        putStrLn "Received pack file"
        return (mapMaybe toRef pack, B.drop 8 rawPack)
