{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module    : GameOfLife
-- Copyright : [2018..] Trevor L. McDonell
-- License   : BSD3
--
-- Conway's Game of Life
-- <https://en.wikipedia.org/wiki/Conway's_Game_of_Life>
--

module GameOfLife ( gameoflife )
  where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits

import qualified Data.Bits                                as P
import qualified Prelude                                  as P
import Prelude                                            ( otherwise )


gameoflife
    :: (Num a, Bits a, P.FiniteBits a, FromIntegral Int a)
    => Acc (Matrix a)
    -> Acc (Matrix a)
gameoflife = stencil gol wrap

-- The stencil pattern, where each cell is represented by a single bit.
-- Thus this computation is actually an embedded b-wide tiled stencil, for
-- base types of b-bits wide (typically, a 32-bit word). Each cell is
-- computed using bit-wise operations, and should be relatively efficient.
--
gol :: forall a. (Num a, Bits a, P.FiniteBits a, FromIntegral Int a)
    => Stencil3x3 a
    -> Exp a
gol ((nw,n,ne)
    ,( w,c,e )
    ,(sw,s,se)) = P.foldl (.|.) 0 [ go b | b <- [0 .. bits-1] ]
  where
    bits  = P.finiteBitSize (undefined :: a)

    evolve :: Int -> Exp Int -> Exp a
    evolve (constant -> i) neighbours =
      let alive = testBit c i
          next  = (alive && neighbours == 2) || neighbours == 3
      in
      (0b1 `shiftL` i) * fromIntegral (boolToInt next)

    go :: Int -> Exp a
    go b = evolve b neighbours
      where
        neighbours
          | b P.== 0
          = popCount (n .&. 0b11)   + boolToInt (testBit ne (constant bits-1))
          + boolToInt (testBit c 1) + boolToInt (testBit e  (constant bits-1))
          + popCount (s .&. 0b11)   + boolToInt (testBit se (constant bits-1))

          | b P.== (bits-1)
          = boolToInt (testBit nw 0) + popCount (n .&. (0b11 `shiftL` (constant bits-2)))
          + boolToInt (testBit  w 0) + boolToInt (testBit c (constant bits-2))
          + boolToInt (testBit sw 0) + popCount (s .&. (0b11 `shiftL` (constant bits-2)))

          | otherwise
          = popCount (n .&. (0b111 `shiftL` (constant b-1)))
          + popCount (c .&. (0b101 `shiftL` (constant b-1)))
          + popCount (s .&. (0b111 `shiftL` (constant b-1)))

