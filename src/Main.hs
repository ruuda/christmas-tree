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
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Hardware.Serialport

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Builder as ByteString

import Debug.Trace (traceShow)

type Color = (Float, Float, Float)

mapColor :: (Float -> a) -> Color -> (a, a, a)
mapColor f (r, g, b) = (f r, f g, f b)

serializeColor :: Color -> ByteString.Builder
serializeColor color =
  let
    -- The LEDs have quite a non-linear response, so square the values to
    -- counter that a bit.
    correctPower x = x * x
    toByte x = round (255.0 * x)
    (r, g, b) = mapColor (ByteString.word8 . toByte . correctPower) color
  in
    -- The data is just the three RGB bytes.
    r <> g <> b

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
    let
      colors = take 25 $ cycle [
        (1.0, 0.0, 0.0),
        (1.0, 1.0, 0.0),
        (0.0, 1.0, 0.0),
        (0.0, 1.0, 1.0),
        (0.0, 0.0, 1.0)]
    sendDatagram sp $ buildDatagram colors
