{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- FIXME implement ls-remote because we already have it anyway

module Git.Remote(
    clone
  , parseRemote
  , Remote(..)
) where

import qualified Data.Attoparsec.Char8 as AC
import Data.Attoparsec.Combinator
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Control.Applicative                      ((<$>))
import System.Directory                         (removeFile, createDirectoryIfMissing)
import System.FilePath                          ((</>), takeFileName, dropExtension)
import Network.Socket                           (withSocketsDo)
import Data.Maybe
import Data.List
import Git.Common
import Git.TcpClient
import Git.PackProtocol
import Git.Store.ObjectStore

refDiscovery :: String -> String -> String
refDiscovery host repo = pktLine $ "git-upload-pack /" ++ repo ++ "\0host="++host++"\0" -- ++ flushPkt -- Tell the server to disconnect

toObjId :: PacketLine -> Maybe String
toObjId (FirstLine obj _ _) = Just $ C.unpack obj
toObjId (RefLine obj _)     = Just $ C.unpack obj
toObjId _                   = Nothing


-- PKT-LINE("want" SP obj-id SP capability-list LF)
-- PKT-LINE("want" SP obj-id LF)
--
-- FIXME - filter heads/tags
createNegotiationRequest :: [String] -> [PacketLine] -> String
createNegotiationRequest capabilities = concatMap (++ "") . nub . map (pktLine . (++ "\n")) . foldl' (\acc e -> if null acc then first acc e else additional acc e) [] . wants
                    where wants = mapMaybe toObjId
                          first acc obj      = acc ++ ["want " ++ obj ++ " " ++ unwords capabilities]
                          additional acc obj = acc ++ ["want " ++ obj]

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
                       in clone' (GitRepository gitRepoName (gitRepoName </> ".git")) remote
        _           -> putStrLn $ "Invalid URL" ++ url


-- .git/objects/pack/tmp_pack_6bo2La
clone' :: GitRepository -> Remote -> IO ()
clone' repo Remote{..} = withSocketsDo $
    withConnection getHost (show $ fromMaybe 9418 getPort) $ \sock -> do
        let payload = refDiscovery getHost getRepository
        send sock payload
        putStrLn "Receiving..."
        response <- receive sock
        let pack = parsePacket $ L.fromChunks [response]
            --wants = map toObjId pack
        let request = createNegotiationRequest ["multi_ack_detailed", "agent=git/1.8.1"] pack ++ flushPkt ++ "0009done\n"
        {-let request = (createNegotiationRequest ["multi_ack_detailed", "side-band-64k", "thin-pack", "ofs-delta", "agent=git/1.8.1"] pack) ++ flushPkt ++ "0009done\n"-}
        send sock request
        response2 <- receiveFully sock
        putStrLn "Received bytes:"
        print $ B.length response2
        let packFile = B.drop 8 response2
            dir = pathForPack repo
            tmpPack = dir </> "tmp_pack_incoming"
        _ <- createDirectoryIfMissing True dir
        B.writeFile tmpPack packFile
        createGitRepositoryFromPackfile repo tmpPack
        removeFile tmpPack
        putStrLn "Finished"
