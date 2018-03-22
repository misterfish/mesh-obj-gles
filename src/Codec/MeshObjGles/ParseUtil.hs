module Codec.MeshObjGles.ParseUtil ( trim
                                   , frint
                                   , verple2
                                   , verple3
                                   , verl2
                                   , verl3
                                   , dec
                                   , asterisk
                                   , fst3
                                   , snd3
                                   , thd3 ) where

import           Data.Functor.Identity ( Identity )
import           Data.Char ( isSpace )
import           Text.Parsec ( ParsecT, oneOf )

import           Codec.MeshObjGles.Types ( Vertex2 (Vertex2)
                                         , Vertex3 (Vertex3) )

type Parser = ParsecT String () Identity

-- | not efficient.
trim :: String -> String
trim = reverse . trim' . reverse . trim' where
    trim' "" = ""
    trim' (x:xs)
      | isSpace x = trim xs
      | otherwise = x:xs

frint = fromIntegral
verple2 (a, b) = Vertex2 a b
verple3 (a, b, c) = Vertex3 a b c
verl2 [a, b] = Vertex2 a b
verl3 [a, b, c] = Vertex3 a b c

dec :: Integral a => a -> a
dec = (+ (-1))

fs `asterisk` x = map map' fs where map' f = f x

fst3 (a, b, c) = a
snd3 (a, b, c) = b
thd3 (a, b, c) = c

