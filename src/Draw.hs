{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module    : Draw
-- Copyright : [2018..] Trevor L. McDonell
-- License   : BSD3
--

module Draw where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Data.Colour.RGBA
import Data.Array.Accelerate.Data.Colour.Names            as A

import qualified Data.Bits                                as P
import qualified Prelude                                  as P


-- Generate a bitmap image in RGBA format for the given viewport. This
-- could probably be done much more efficiently, decoding a word at a time.
-- Currently this phase takes twice as long as the update step for
-- turing_js_r.rle.
--
rgbaOfBitmap
    :: forall a. (Bits a, P.FiniteBits a)
    => Int
    -> Int
    -> Acc (Scalar (Int,Int,Int,Int))
    -> Acc (Matrix a)
    -> Acc (Matrix (RGBA Word8))
rgbaOfBitmap screenW screenH viewport bitmap =
  let sh    = constant (Z :. screenH :. screenW)
      bits  = P.finiteBitSize (undefined :: a)
  in
  generate sh $ \(I2 y x) ->
    let
        I2 simY simX           = shape bitmap
        T4 minX minY maxX maxY = the viewport

        viewX   = maxX - minX
        scaleX  = fromIntegral viewX / P.fromIntegral screenW :: Exp Float
        px      = minX + floor (fromIntegral x * scaleX)

        viewY   = maxY - minY
        scaleY  = fromIntegral viewY / P.fromIntegral screenH :: Exp Float
        py      = simY - minY - floor (fromIntegral y * scaleY) - 1

        (wx,bx) = px `quotRem` constant bits
        u       = bitmap ! I2 py wx
        v       = testBit u (constant bits - bx - 1)
     in
     if py < 0 || py >= simY || px < 0 || wx >= simX
        then A.black
        else if v then A.white
                  else A.black

