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
import Data.Fixed (mod')
import Data.Monoid ((<>))
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Hardware.Serialport

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import qualified Data.ByteString.Builder as ByteString
import qualified Data.Time.Clock as Clock

import Debug.Trace (traceShow)

import Protocol (Color, Mode (..))

mapColor :: (Float -> a) -> Color -> (a, a, a)
mapColor f (r, g, b) = (f r, f g, f b)

-- Create a color from hue, saturation, and lightness. All values are in the
-- range 0-1.
hsl :: Float -> Float -> Float -> Color
hsl h s l = (r + m, g + m, b + m)
  where
    wrap x = x - (fromInteger (floor x))
    h' = h `mod'` 1
    c = s * (1 - abs (2 * l - 1))
    x = c * (1 - abs (((h * 6) `mod'` 2) - 1))
    m = l - 0.5 * c
    (r, g, b) | h' < 1/6 = (c, x, 0)
              | h' < 2/6 = (x, c, 0)
              | h' < 3/6 = (0, c, x)
              | h' < 4/6 = (0, x, c)
              | h' < 5/6 = (x, 0, c)
              | otherwise = (c, 0, x)

-- Compute the color for each of the 25 LEDs, given the time and mode.
assignColors :: Float -> Mode -> [Color]
assignColors t mode = fmap f [1..25]
  where
    f i = case mode of
      Rainbow ->
        hsl (0.1 * fromIntegral i) 1.0 0.5
      RainbowCycle ->
        hsl (t * 0.05 + 0.1 * fromIntegral i) 1.0 0.5
      GoldCycle ->
        hsl 0.12 0.5 (((sin (t * 0.1 + 0.2 * fromIntegral i)) ^ 2) * 0.7)
      Blink color ->
        if (t `mod'` 1) < 0.5 then color else (0, 0, 0)

getSecondsSinceMidnight :: IO Float
getSecondsSinceMidnight = fmap toSecs Clock.getCurrentTime
  where
    toSecs = (* 1.0e-12) . fromIntegral . Clock.diffTimeToPicoseconds . Clock.utctDayTime

serializeColor :: Color -> ByteString.Builder
serializeColor color =
  let
    -- The LEDs have quite a non-linear response, so cube the values to
    -- counter that a bit.
    correctPower x = x * x * x
    toByte x = ByteString.word8 (round (255.0 * x))
    (r, g, b) = mapColor (toByte . correctPower) color
  in
    -- The data is just the three RGB bytes.
    r <> g <> b

buildDatagram :: [Color] -> ByteString.Builder
buildDatagram colors =
  let
    numLeds = length colors
    magicBytes = ByteString.byteString "Ada"
    numLedsBytes = ByteString.word16LE (fromIntegral (numLeds - 1))
    -- This checksum is only correct for less than 256 LEDs, but I have only 25
    -- anyway.
    checksumBytes = ByteString.word8 (fromIntegral (numLeds - 1) `xor` 0x55)
    colorBytes = mconcat (fmap serializeColor colors)
  in
    magicBytes <> numLedsBytes <> checksumBytes <> colorBytes

sendDatagram :: SerialPort -> ByteString.Builder -> IO ()
sendDatagram port dgram = do
  send port $! ByteString.toStrict $ ByteString.toLazyByteString dgram
  flush port

parseCommandLine :: IO (String, String)
parseCommandLine = do
  args <- getArgs
  case args of
    portAddr : hostname : [] -> pure (portAddr, hostname)
    _ -> do
      putStrLn "Usage: christmas-tree PORT HOSTNAME"
      putStrLn ""
      putStrLn "  PORT:     Arduino port ('COM3' on Windows, '/dev/ttyUSB0' on Linux)."
      putStrLn "  HOSTNAME: Server to get mode updates from."
      exitFailure

main :: IO ()
main = do
  let settings = defaultSerialSettings { commSpeed = CS115200 }
  (portAddr, _hostname) <- parseCommandLine
  sp <- openSerial portAddr settings
  forever $ do
    t <- getSecondsSinceMidnight
    sendDatagram sp $ buildDatagram (assignColors t GoldCycle)
