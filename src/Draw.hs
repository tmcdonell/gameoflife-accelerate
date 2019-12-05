{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module    : Draw
-- Copyright : [2018..] Trevor L. McDonell
-- License   : BSD3
--

module Draw where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Data.Colour.RGBA
import Data.Array.Accelerate.Data.Colour.Names

import qualified Data.Bits                                as P


rgbaOfBitmap
    :: forall a. (Bits a, P.FiniteBits a)
    => Acc (Matrix a)
    -> Acc (Matrix (RGBA Word8))
rgbaOfBitmap bm =
  let I2 height width = shape bm
      sh              = I2 height (width * constant bits)
      bits            = P.finiteBitSize (undefined :: a)
  in
  generate sh $ \(I2 y x) ->
    let (w, b) = x `quotRem` constant bits
        u      = bm ! I2 (height - y - 1) w
        v      = testBit u (constant bits - b - 1)
     in
     if v then white
          else black

