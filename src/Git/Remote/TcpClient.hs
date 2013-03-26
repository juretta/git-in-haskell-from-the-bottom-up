{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

-- | A git compatible TcpClient that understands the git packet line format.
module Git.Remote.TcpClient (
   withConnection
 , send
 , receiveWithSideband
 , receiveFully
 , receive
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
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
            if C.null msg then return acc else receive' s $ acc `mappend` msg


-- see sideband.c
-- FIXME: the git client prepends "remote: " to the output coming from the
-- remote
receiveWithSideband :: Socket -> (B.ByteString -> IO a) -> IO B.ByteString
receiveWithSideband sock f = recrec mempty
    where recrec acc = do
            !maybeLine <- readPacketLine sock
            let skip = recrec acc
            case maybeLine of
                Just "NAK\n" -> skip -- ignore here...
                Just line -> case B.uncons line of
                                Just (1, rest)  -> recrec (acc `mappend` rest)
                                Just (2, rest)  -> f ("remote: " `C.append` rest) >> skip -- FIXME - scan for linebreaks and prepend "remote: " accordingly (see sideband.c)
                                Just (_, rest)  -> fail $ C.unpack rest
                                Nothing         -> skip
                Nothing   -> return acc

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

-- | Read a git packet line (variable length binary string prefixed with the overall length). 
-- Length is 4 byte, hexadecimal, padded with 0.
readPacketLine :: Socket -> IO (Maybe C.ByteString)
readPacketLine sock = do
        len <- readFully mempty 4 -- check for a zero length return -> disconnected
        if C.null len then return Nothing else
            case readHex $ C.unpack len of
                ((l,_):_) | l > 4 -> do
                     line <- readFully mempty (l-4)
                     return $ Just line
                _ -> return Nothing
    where readFully acc expected = do
            line <- recv sock expected
            let len  = C.length line
                acc' = acc `mappend` line
                cont = len /= expected && not (C.null line)
            if cont then readFully acc' (expected - len) else return acc'

{-
If 'side-band' or 'side-band-64k' capabilities have been specified by
the client, the server will send the packfile data multiplexed.

Each packet starting with the packet-line length of the amount of data
that follows, followed by a single byte specifying the sideband the
following data is coming in on.

In 'side-band' mode, it will send up to 999 data bytes plus 1 control
code, for a total of up to 1000 bytes in a pkt-line.  In 'side-band-64k'
mode it will send up to 65519 data bytes plus 1 control code, for a
total of up to 65520 bytes in a pkt-line.

The sideband byte will be a '1', '2' or a '3'. Sideband '1' will contain
packfile data, sideband '2' will be used for progress information that the
client will generally print to stderr and sideband '3' is used for error
information.

If no 'side-band' capability was specified, the server will stream the
entire packfile without multiplexing.
-}
