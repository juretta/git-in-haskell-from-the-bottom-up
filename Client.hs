module Client where


import Data.Bits
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Network.BSD
import Data.List
import System.IO

type Request = String
type Response = C.ByteString
type Host = String
type Port = Int
{-client host port = -}

{-client :: Host -> Port -> Request -> (Response -> ()) -}

sendViaSocket host port payload = withSocketsDo $
     do addrinfos <- getAddrInfo Nothing (Just host) (Just port)
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        sendAll sock $ C.pack payload
        msg <- receive sock C.empty
        sClose sock
        return msg
    where receive s acc = do
            msg <- recv s 4096
            if C.null msg then return acc else receive s $ C.append acc msg
        
