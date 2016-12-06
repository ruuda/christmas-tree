-- Christmas Tree -- An API for lighting up my Christmas tree
-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. A copy
-- of the License is available in the root of the repository.

module Protocol (Color, Mode (..)) where

type Color = (Float, Float, Float)

type TimeFreq = Float
type SpatialFreq = Float

data Mode
  = Blink Color
  | ColorCycle Color TimeFreq SpatialFreq
  | Rainbow
  | RainbowCycle TimeFreq SpatialFreq
  deriving (Eq, Read, Show)
