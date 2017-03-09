{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async    as Async
import           Control.Monad
import           Data.ByteString             (ByteString)
import           System.Environment
import           System.Random
import           System.Socket               as Sock
import           System.Socket.Family.Inet6  as Inet6
import           System.Socket.Protocol.UDP  as UDP
import           System.Socket.Type.Datagram as Dgram
import           System.Timeout

type UdpSocket inet = Socket inet Dgram.Datagram UDP.UDP

newUdpSocket :: Sock.Family f => Sock.SocketAddress f -> IO (UdpSocket f)
newUdpSocket bindAddress = do
    sock <- socket
    setSocketOption sock (ReuseAddress True)
    -- setSocketOption sock (Inet6.V6Only False)
    bind sock bindAddress
    return sock

serverLoop :: (Sock.Family f) => UdpSocket f -> IO ()
serverLoop sock = forever $ do
    (msg, sender) <- receiveFrom sock 65507 mempty
    let msg = "pong" :: ByteString
     in sendTo sock msg mempty sender

sendReceiveLoop :: (Sock.Family f) => Sock.SocketAddress f -> UdpSocket f -> IO ()
sendReceiveLoop serverAddress sock = forever $ do
    let msg = "ping" :: ByteString
    sendTo sock msg mempty serverAddress
    (!response, sender) <- receiveFrom sock 65507 mempty
    delay <- (*) 1000 <$> randomRIO (1, 300)
    threadDelay delay


main :: IO ()
main = do
    -- Get threads num from arguments
    threads <- read . head <$> getArgs

    let serverAddress = Inet6.SocketAddressInet6 Inet6.inet6Any 5252 0 0
        anyAddress = Inet6.SocketAddressInet6 Inet6.inet6Any 0 0 0

    -- Create, configure and bind socket
    server <- newUdpSocket serverAddress
    withAsync (serverLoop server) $ \_ ->
        replicateConcurrently threads
            (newUdpSocket anyAddress >>= sendReceiveLoop serverAddress)
    return ()
