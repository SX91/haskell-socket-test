module Main where

import           Control.Monad
import           System.Environment
import           System.Socket               as Sock
import           System.Socket.Family.Inet6  as Inet6
import           System.Socket.Protocol.UDP  as UDP
import           System.Socket.Type.Datagram as Dgram
import           System.Timeout


main :: IO ()
main = do
    -- Get timeout from arguments
    to <- read . head <$> getArgs

    -- Create, configure and bind socket
    sock <- socket :: IO (Socket Inet6.Inet6 Dgram.Datagram UDP.UDP)
    setSocketOption sock (ReuseAddress True)
    setSocketOption sock (Inet6.V6Only False)
    bind sock (Inet6.SocketAddressInet6 Inet6.inet6Any 0 0 0)

    -- Now lets start receive-abort-receive cycle
    forever $
        timeout to $ receiveFrom sock 65535 mempty
