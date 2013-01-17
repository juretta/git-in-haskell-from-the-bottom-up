-- | A git compatible TcpClient that understands the git packet line format.
module Git.TcpClient (
   withConnection
 , send
 , receiveFully
 , receive
) where

import qualified Data.ByteString.Char8 as C
import Network.Socket hiding                    (recv, send)
import Network.Socket.ByteString                (recv, sendAll)
import Data.Monoid                              (mempty, mappend)
import Numeric                                  (readHex)

withConnection :: HostName -> ServiceName -> (Socket -> IO b) -> IO b
withConnection host port consumer = do
    sock <- openConnection host port
    r <- consumer sock
    sClose sock
    return r


send :: Socket -> String -> IO ()
send sock msg = sendAll sock $ C.pack msg


-- | Read from the socket until the peer closes its half side of the
-- connection.
receiveFully :: Socket -> IO C.ByteString
receiveFully sock = receive' sock mempty
   where receive' s acc = do
            msg <- recv s 4096
            if C.null msg then return acc else receive' s $ mappend acc msg


-- | Read packet lines.
receive :: Socket -> IO C.ByteString
receive sock = receive' sock mempty
    where receive' s acc = do
            maybeLine <- readPacketLine s
            maybe (return acc) (receive' s . mappend acc) maybeLine

-- =================================================================================

openConnection :: HostName -> ServiceName -> IO Socket
openConnection host port = do
        addrinfos <- getAddrInfo Nothing (Just host) (Just port)
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        return sock

-- | Read a packet line
readPacketLine :: Socket -> IO (Maybe C.ByteString)
readPacketLine sock = do
    msg <- recv sock 4 -- check for a zero length return -> disconnected
    if C.null msg then return Nothing else
        case readHex $ C.unpack msg of
            ((l,_):_) | l > 4 -> do
                 line <- recv sock (l-4)
                 return $ Just line
            _ -> return Nothing
