-- Christmas Tree -- An API for lighting up my Christmas tree
-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. A copy
-- of the License is available in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- This module defines a simple API server to control the lights.
-- It can be used like so, for example:
--
--     curl -d '' https://chainsaw:pass@example.com/blink?color=#ff0000&duration=5s

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.SecureMem (SecureMem, secureMemFromByteString)
import Network.Socket (Socket)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import System.IO (BufferMode (LineBuffering), Handle, hSetBuffering, stderr, stdout)
import System.Posix.Env (getEnv)
import Web.Scotty (ScottyM, middleware, param, post, scottyApp, text)

import qualified Data.ByteString.Char8 as ByteString
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBs
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

isAuthOk :: SecureMem -> ByteString -> ByteString -> Bool
isAuthOk actualPassword user presumedPassword =
  user == "chainsaw" &&
  -- Use SecureMem for a constant-time string comparison.
  actualPassword == (secureMemFromByteString presumedPassword)

server :: SecureMem -> ScottyM ()
server password = do

  middleware $ basicAuth (\ u p -> pure (isAuthOk password u p)) "chainsaw"

  post "/blink" $ do
    --color <- param "color"
    --duration <- param "duration"
    text "blinking it is!"

feedClient :: Socket -> IO ()
feedClient socket = do
  void $ SocketBs.send socket "hi"

acceptClients :: Socket -> IO ()
acceptClients socket = go
  where
    go = do
      (clientSocket, clientAddr) <- Socket.accept socket
      putStrLn $ "Incoming socket connection from " ++ (show clientAddr)
      forkIO (feedClient clientSocket)
      go

runSocketServer :: Int -> IO ()
runSocketServer port = do
  let localhost = Socket.tupleToHostAddress (127, 0, 0, 1)
  socket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.bind socket $ Socket.SockAddrInet (fromIntegral port) localhost
  Socket.listen socket 1
  acceptClients socket

runHttpsServer :: FilePath -> FilePath -> Int -> String -> IO ()
runHttpsServer certPath skeyPath port password =
  let
    tlsSettings = Warp.tlsSettings certPath skeyPath
    httpSettings = Warp.setPort port Warp.defaultSettings
    passwordSecMem = secureMemFromByteString $ ByteString.pack password
  in
    Warp.runTLS tlsSettings httpSettings =<< (scottyApp $ server passwordSecMem)

justGetEnv :: String -> IO String
justGetEnv varname = do
  maybeValue <- getEnv varname
  case maybeValue of
    Nothing -> error (varname ++ " not set")
    Just value -> pure value

main = do
  -- Read configuration from environment variables.
  certPath <- justGetEnv "CERT_PATH"
  skeyPath <- justGetEnv "SECRET_KEY_PATH"
  password <- justGetEnv "PASSWORD"
  httpPort <- fmap read $ justGetEnv "HTTP_PORT" :: IO Int
  sockPort <- fmap read $ justGetEnv "SOCK_PORT" :: IO Int

  -- When the runtime detects that stdout is not connected to a console, it
  -- defaults to block buffering instead of line buffering. When running under
  -- systemd, this prevents things written to stdout from showing up until the
  -- buffer is full. Therefore, explicitly select line buffering, to enforce a
  -- flush after every newline.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  putStrLn $ "Starting socket server at port " ++ (show sockPort)
  void $ forkIO $ runSocketServer sockPort

  putStrLn $ "Starting https server at port " ++ (show httpPort)
  runHttpsServer certPath skeyPath httpPort password
