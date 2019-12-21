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
import GameOfLife

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Data.Bits                    as A
import Data.Array.Accelerate.Data.Colour.RGBA             as A
import Data.Array.Accelerate.Interpreter                  as I

import Graphics.Gloss.Accelerate.Data.Picture
import Graphics.Gloss.Interface.Pure.Simulate

import qualified Data.Bits                                as P
import qualified Prelude                                  as P
import Prelude                                            ( IO )


main :: IO ()
main = do
  let
      input       = glider1

      Z :. h :. w = arrayShape input
      width       = w * bits input  * P.round factor
      height      = h * P.round factor
      fps         = 5
      factor      = 20

      bits :: forall sh a. P.FiniteBits a => Array sh a -> Int
      bits _ = P.finiteBitSize (undefined::a)

      disp :: (Bits a, P.FiniteBits a) => Matrix a -> Picture
      disp x    =
        let go = runN $ A.map packRGBA8 . rgbaOfBitmap
         in scale factor factor $ bitmapOfArray (go x) True

      step :: (Num a, Bits a, P.FiniteBits a, FromIntegral Int a)
           => viewport -> time -> Matrix a -> Matrix a
      step _ _  = runN gameoflife

  simulate
    (InWindow "Game of Life" (width, height) (10,10))
    black
    fps
    input
    disp
    step

