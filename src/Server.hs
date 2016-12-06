-- Christmas Tree -- An API for lighting up my Christmas tree
-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. A copy
-- of the License is available in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

-- This module defines a simple API server to control the lights.
-- It can be used like so, for example:
--
--     curl -d '' https://chainsaw:pass@example.com/blink?color=ff0000&duration=5

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan)
import Control.Monad (void, when)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.SecureMem (SecureMem, secureMemFromByteString)
import Network.Socket (Socket)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Environment (lookupEnv)
import Web.Scotty (ScottyM, liftAndCatchIO, middleware, post, scottyApp)

import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Attoparsec.Text as Ap
import qualified Data.Text.Lazy as Text
import qualified Network.Simple.TCP as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Web.Scotty as Scotty

import Protocol (Color, Mode (..))

data Cmd
  = Delay Int -- A delay in seconds.
  | PopMode
  | PushMode Mode
  | SetMode Mode

colorFromInt :: Int -> Color
colorFromInt x =
  let
    ri = (x `shiftR` 16) .&. 0xff
    rg = (x `shiftR` 8) .&. 0xff
    rb = (x `shiftR` 0) .&. 0xff
  in
    ((fromIntegral ri) / 255.0, (fromIntegral rg) / 255.0, (fromIntegral rb) / 255.0)

-- Parse colors from web-style hex colors like ff00ff (note the missing #, that
-- one is awkward to use in urls).
instance Scotty.Parsable (Float, Float, Float) where
  parseParam text =
    let
      parser = fmap colorFromInt (Ap.hexadecimal <* Ap.endOfInput)
    in
      case Ap.parseOnly parser (Text.toStrict text) of
        Left err -> Left (Text.pack err)
        Right color -> Right color

isAuthOk :: SecureMem -> ByteString -> ByteString -> Bool
isAuthOk actualPassword user presumedPassword =
  user == "chainsaw" &&
  -- Use SecureMem for a constant-time string comparison.
  actualPassword == (secureMemFromByteString presumedPassword)

server :: SecureMem -> Chan Cmd -> ScottyM ()
server password chan =
  let
    sendCmds cmds = liftAndCatchIO $ mapM_ (Chan.writeChan chan) cmds
    paramOrDefault name def = Scotty.param name `Scotty.rescue` (const (pure def))
  in do

  middleware $ basicAuth (\ u p -> pure (isAuthOk password u p)) "chainsaw"

  post "/blink" $ do
    color <- Scotty.param "color"
    duration <- Scotty.param "duration"
    sendCmds [PushMode $ Blink color, Delay duration, PopMode]
    Scotty.text "Blinking it is!\n"

  post "/rainbow" $ do
    sendCmds [SetMode Rainbow]
    Scotty.text "Rainbows ok. No ponies though.\n"

  post "/rainbow-cycle" $ do
    tfreq <- paramOrDefault "tfreq" 0.05
    sfreq <- paramOrDefault "sfreq" 0.1
    sendCmds [SetMode $ RainbowCycle tfreq sfreq]
    Scotty.text "Such animated rainbow. Very wow.\n"

  post "/gold-cycle" $ do
    tfreq <- paramOrDefault "tfreq" 0.1
    sfreq <- paramOrDefault "sfreq" 0.3
    sendCmds [SetMode $ ColorCycle (1.0, 0.7, 0.2) tfreq sfreq]
    Scotty.text "A most excellent choice.\n"

  post "/color-cycle" $ do
    color <- Scotty.param "color"
    tfreq <- paramOrDefault "tfreq" 0.1
    sfreq <- paramOrDefault "sfreq" 0.3
    sendCmds [SetMode $ ColorCycle color tfreq sfreq]
    Scotty.text "I just hope it's not pink.\n"

-- Send the current mode to the client over the socket.
sendMode :: Socket -> Mode -> IO ()
sendMode socket mode = do
  putStrLn $ "Sending new mode: " ++ (show mode) ++ "."
  Socket.send socket (ByteString.pack ((show mode) ++ "\n"))

feedClient :: Chan Cmd -> Socket -> IO ()
feedClient chan socket = (sendMode socket initialMode) >> (go [initialMode])
  where
    initialMode = Rainbow
    -- The function maintains a stack of modes, and commands manipulate this
    -- stack. This allows for example blinking for a few seconds, and then going
    -- back to whichever mode was active before.
    go modeStack = do
      -- Handle all commands that come in through the channel in a loop.
      cmd <- Chan.readChan chan
      modeStack' <- case cmd of
        Delay secs -> (threadDelay (1000 * 1000 * secs)) >> (pure modeStack)
        PopMode -> pure (tail modeStack)
        PushMode mode -> pure (mode : modeStack)
        SetMode mode -> pure (mode : tail modeStack)

      -- Send the mode which is currently on the top of the stack to the client,
      -- if it changed.
      when (modeStack' /= modeStack) $ sendMode socket (head modeStack')

      -- Recurse to loop.
      go modeStack'

runSocketServer :: Int -> Chan Cmd -> IO ()
runSocketServer port chan =
  Socket.serve Socket.HostAny (show port) $ \ (socket, clientAddr) -> do
    putStrLn $ "Incoming socket connection from " ++ (show clientAddr)
    clientChan <- Chan.dupChan chan
    feedClient clientChan socket

runHttpsServer :: FilePath -> FilePath -> Int -> String -> Chan Cmd -> IO ()
runHttpsServer certPath skeyPath port password chan =
  let
    tlsSettings = Warp.tlsSettings certPath skeyPath
    httpSettings = Warp.setPort port Warp.defaultSettings
    passwordSecMem = secureMemFromByteString $ ByteString.pack password
  in
    Warp.runTLS tlsSettings httpSettings =<< (scottyApp $ server passwordSecMem chan)

justGetEnv :: String -> IO String
justGetEnv varname = do
  maybeValue <- lookupEnv varname
  case maybeValue of
    Nothing -> error (varname ++ " not set")
    Just value -> pure value

main :: IO ()
main = do
  -- When the runtime detects that stdout is not connected to a console, it
  -- defaults to block buffering instead of line buffering. When running under
  -- systemd, this prevents things written to stdout from showing up until the
  -- buffer is full. Therefore, explicitly select line buffering, to enforce a
  -- flush after every newline.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Read configuration from environment variables.
  certPath <- justGetEnv "CERT_PATH"
  skeyPath <- justGetEnv "SECRET_KEY_PATH"
  password <- justGetEnv "PASSWORD"
  httpPort <- fmap read $ justGetEnv "HTTP_PORT" :: IO Int
  sockPort <- fmap read $ justGetEnv "SOCK_PORT" :: IO Int

  -- The API server will push commands to change the mode into this channel.
  chan <- Chan.newChan

  putStrLn $ "Starting socket server at port " ++ (show sockPort)
  void $ forkIO $ runSocketServer sockPort chan

  putStrLn $ "Starting https server at port " ++ (show httpPort)
  runHttpsServer certPath skeyPath httpPort password chan
