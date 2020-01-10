{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module    : Main
-- Copyright : [2018..] Trevor L. McDonell
-- License   : BSD3
--

module Main where

import Draw
import Petri
import Parser
import GameOfLife

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Data.Bits                    as A
import Data.Array.Accelerate.Data.Colour.RGBA             as A
-- import Data.Array.Accelerate.Interpreter                  as I
import Data.Array.Accelerate.LLVM.Native                  as CPU
-- import Data.Array.Accelerate.LLVM.PTX                     as GPU

import Graphics.Gloss.Accelerate.Data.Picture
import Graphics.Gloss.Interface.Pure.Simulate

import qualified Data.Bits                                as P
import qualified Prelude                                  as P
import Prelude                                            ( IO, (<$>) )


main :: IO ()
main = do
  -- input <- bitmapOfGolly <$> parseGolly "samples/c4-orthogonal.rle" :: IO (Matrix Word32)
  input <- bitmapOfGolly <$> parseGolly "samples/turing_js_r.rle" :: IO (Matrix Word32)
  -- input <- bitmapOfGolly <$> parseGolly "samples/metapixel-galaxy.rle" :: IO (Matrix Word32)

  let
      Z :. h :. w = arrayShape input
      width       = w * bits input  * P.round factor
      height      = h * P.round factor
      fps         = 15
      factor      = 1 --20

      bits :: forall sh a. P.FiniteBits a => Array sh a -> Int
      bits _ = P.finiteBitSize (undefined::a)

      disp = runN (A.map packRGBA8 . rgbaOfBitmap)
      step = runN gameoflife

  simulate
    (InWindow "Game of Life" (width, height) (10,10))
    black
    fps
    input
    (\x     -> bitmapOfArray (disp x) False)
    (\_ _ x -> step x)

