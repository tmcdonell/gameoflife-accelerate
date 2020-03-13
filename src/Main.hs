{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module    : Main
-- Copyright : [2018..] Trevor L. McDonell
-- License   : BSD3
--

module Main where

import Draw
import Parser
import GameOfLife

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Data.Colour.RGBA             as A
import Data.Array.Accelerate.LLVM.Native                  as CPU
-- import Data.Array.Accelerate.LLVM.PTX                     as GPU

import Graphics.Gloss.Accelerate.Data.Picture
import Graphics.Gloss.Interface.Pure.Simulate

import qualified Data.Bits                                as P
import qualified Prelude                                  as P
import Prelude                                            ( (<$>), IO )


main :: IO ()
main = do

  -- input <- bitmapOfGolly <$> parseGolly "samples/c4-orthogonal.rle" :: IO (Matrix Word32)
  input <- bitmapOfGolly <$> parseGolly "samples/turing_js_r.rle" :: IO (Matrix Word32)
  -- input <- bitmapOfGolly <$> parseGolly "samples/metapixel-galaxy.rle" :: IO (Matrix Word32)

  let
      Z :. simY :. wX   = arrayShape input
      simX              = wX * bits input
      screenX           = 1024
      screenY           = 768
      fps               = 10
      --
      step
        = runN
        $ \v x -> let x' = gameoflife x
                      bm = A.map packRGBA8 $ rgbaOfBitmap screenX screenY v x'
                   in T2 x' bm

  simulate
    (InWindow "Game of Life" (screenX, screenY) (10,10))
    black
    fps
    (input, blank)
    P.snd
    (\ViewPort{..} _ (x,_) ->
      let
          (tx, ty)  = viewPortTranslate
          midX      = (P.fromIntegral simX / 2) - tx
          midY      = (P.fromIntegral simY / 2) - ty

          dx        = (P.fromIntegral screenX / 2) / viewPortScale
          dy        = (P.fromIntegral screenY / 2) / viewPortScale

          v         = fromList Z [ (P.floor   (midX - dx), P.floor   (midY - dy)
                                   ,P.ceiling (midX + dx), P.ceiling (midY + dy)) ]

          (x', p) = step v x
       in
       (x', translate (-tx) (-ty) $ bitmapOfArray p False))

bits :: forall sh a. P.FiniteBits a => Array sh a -> Int
bits _ = P.finiteBitSize (undefined::a)

