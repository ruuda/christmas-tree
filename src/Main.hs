-- Christmas Tree -- An API for lighting up my Christmas tree
-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. A copy
-- of the License is available in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.Bits (xor)
import Data.Monoid ((<>))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Hardware.Serialport

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Builder as ByteString

import qualified Data.Prizm.Types as Color
import qualified Data.Prizm.Color.CIE.LCH as Color

type Color = Color.CIELCH Double

serializeColor :: Color -> ByteString.Builder
serializeColor color =
  let
    Color.RGB r g b = Color.toRGB color
  in
    -- Just the three RGB bytes.
    mconcat $ fmap (ByteString.word8 . fromIntegral) [r, g, b]

buildDatagram :: [Color] -> ByteString.Builder
buildDatagram colors =
  let
    numLeds = length colors
    magicBytes = ByteString.byteString "Ada"
    numLedsBytes = ByteString.word16LE (fromIntegral numLeds)
    -- This checksum is only correct for less than 256 LEDs, but I have only 25
    -- anyway.
    checksumBytes = ByteString.word8 (fromIntegral numLeds `xor` 0x55)
    colorBytes = mconcat (fmap serializeColor colors)
  in
    magicBytes <> numLedsBytes <> checksumBytes <> colorBytes

sendDatagram :: SerialPort -> ByteString.Builder -> IO ()
sendDatagram port dgram = do
  send port $ ByteString.toStrict $ ByteString.toLazyByteString dgram
  flush port

parseCommandLine :: IO String
parseCommandLine = do
  args <- getArgs
  case args of
    portAddr : [] -> pure portAddr
    _ -> do
      putStrLn "Usage: christmas-tree PORT"
      putStrLn ""
      putStrLn "  PORT: Arduino port ('COM3' on Windows, '/dev/ttyUSB0' on Linux)."
      exitFailure

main :: IO ()
main = do
  let settings = defaultSerialSettings { commSpeed = CS115200 }
  portAddr <- parseCommandLine
  sp <- openSerial portAddr settings
  forever $ do
    putStrLn "Setting color ..."
    let colors = take 25 $ cycle $ fmap Color.fromHex ["#ff0000", "#ffff00", "#00ff00", "#00ffff", "#0000ff"]
    sendDatagram sp $ buildDatagram colors
