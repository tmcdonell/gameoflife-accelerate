{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Parser
-- Copyright : [2018..] Trevor L. McDonell
-- License   : BSD3
--

module Parser (

  parseGolly,
  arrayOfGolly, bitmapOfGolly,

) where

import Data.Char
import Data.Void
import Control.Monad                                      ( void )
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer               as L
import Prelude                                            as P

import Data.Text.Lazy                                     ( Text )
import qualified Data.Text.Lazy.IO                        as T

import Data.Array.Accelerate                              as A hiding ( size, not )
import Data.Array.Accelerate.Data.Bits                    as A


parseGolly :: FilePath -> IO Golly
parseGolly file = do
  r <- runParser golly file `fmap` T.readFile file
  case r of
    Left  e -> error (errorBundlePretty e)
    Right g -> return g

arrayOfGolly :: A.Integral e => Golly -> Matrix e
arrayOfGolly = undefined

bitmapOfGolly :: (A.Integral e, A.FiniteBits e) => Golly -> Matrix e
bitmapOfGolly = undefined


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

