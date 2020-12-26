{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module    : Parser
-- Copyright : [2018..] Trevor L. McDonell
-- License   : BSD3
--

module Parser (

  parseGolly,
  arrayOfGolly, bitmapOfGolly,

) where

import Control.Monad                                                ( void )
import Data.Char
import Data.Void
import System.IO.Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Bits                                          as P
import qualified Text.Megaparsec.Char.Lexer                         as L
import Prelude                                                      as P

import Data.Text.Lazy                                               ( Text )
import qualified Data.Text.Lazy.IO                                  as T

import qualified Data.Array.Accelerate                              as A
import qualified Data.Array.Accelerate.Array.Data                   as A
import qualified Data.Array.Accelerate.Sugar.Array                  as A
import qualified Data.Array.Accelerate.Sugar.Elt                    as A
import qualified Data.Array.Accelerate.Representation.Array         as R


parseGolly :: FilePath -> IO Golly
parseGolly file = do
  r <- runParser golly file `fmap` T.readFile file
  case r of
    Left  e -> error (errorBundlePretty e)
    Right g -> return g

arrayOfGolly :: forall e. (P.Integral e, A.Elt e) => Golly -> A.Matrix e
arrayOfGolly (Golly w h pat) = unsafePerformIO $ do
  let
      eR      = A.eltR @e
      sz      = w * h

  adata <- A.newArrayData eR sz
  let
      memset !i
        | i >= sz   = return ()
        | otherwise = A.writeArrayData eR adata i (A.fromElt (0 :: e)) >> memset (i+1)

      write y x =
        A.writeArrayData eR adata (y*w + x) (A.fromElt (1 :: e))

      go _ _ []         = return ()
      go x y (P n s:ss) =
        case s of
          N   -> go 0 (y+n) ss
          S 0 -> go (x+n) y ss
          S 1 -> mapM (write y) [x .. x+n-1] >> go (x+n) y ss
          S _ -> error "bitmapOfGolly: only handles binary states"

  memset 0
  go 0 0 pat
  return $! A.Array (R.Array (((), h), w) adata)

bitmapOfGolly :: forall e. (P.Integral e, P.FiniteBits e, A.Elt e) => Golly -> A.Matrix e
bitmapOfGolly (Golly w h pat) = unsafePerformIO $ do
  let
      eR      = A.eltR @e
      bits    = P.finiteBitSize (undefined::e)
      sz      = w' * h
      w'      = let (q,r) = quotRem w bits
                 in if r == 0
                       then q
                       else q+1

  adata <- A.newArrayData eR sz
  let
      memset !i
        | i >= sz   = return ()
        | otherwise = A.writeArrayData eR adata i (A.fromElt (0::e)) >> memset (i+1)

      -- Since this is run length encoded we should make this more
      -- efficient and write entire words at a time
      write y x = do
        let (q,r) = quotRem x bits
            i     = y * w' + q
        --
        v <- A.readArrayData eR adata i
        A.writeArrayData eR adata i (A.fromElt (P.setBit (A.toElt v :: e) (bits - r - 1)))

      go _ _ []         = return ()
      go x y (P n s:ss) =
        case s of
          N   -> go 0 (y+n) ss
          S 0 -> go (x+n) y ss
          S 1 -> mapM (write y) [x .. x+n-1] >> go (x+n) y ss
          S _ -> error "bitmapOfGolly: only handles binary states"

  memset 0
  go 0 0 pat
  return $! A.Array (R.Array (((), h), w') adata)


-- Parser for the Extended RLE format file used by golly.
-- Right now this only handles binary states and ignores custom rules
--
-- http://golly.sourceforge.net/Help/formats.html#rle
--
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 comment empty

comment :: Parser ()
comment = L.skipLineComment "#" >> eol >> return ()

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

int :: Parser Int
int = lexeme L.decimal

data S = S !Int | N deriving Show
data P = P !Int S   deriving Show

size :: Parser (Int, Int)
size = do
  x <- lexeme (char 'x')      *> lexeme (char '=') *> int <* lexeme (char ',')
  y <- lexeme (char 'y')      *> lexeme (char '=') *> int <* lexeme (char ',')
  _ <- lexeme (string "rule") *> lexeme (char '=') *> takeWhileP Nothing (not . isSpace)
  void eol
  return (x,y)

point :: Parser P
point = P <$> (int <|> pure 1) <*> lexeme (on <|> off <|> eor)
  where
    -- We only care about two states, on and off (plus end of row)
    on  = (char 'o' <|> char 'A') *> pure (S 1)
    off = (char 'b' <|> char '.') *> pure (S 0)
    eor = char '$' *> pure N

data Golly = Golly !Int !Int [P]
  deriving Show

golly :: Parser Golly
golly = do
  void $ many comment
  (width, height) <- size
  pattern_        <- many point
  _               <- void (char '!') <|> eof
  return $ Golly width height pattern_

