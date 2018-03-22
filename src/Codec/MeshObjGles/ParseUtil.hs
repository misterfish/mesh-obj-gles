module Codec.MeshObjGles.ParseUtil ( trim
                 ) where

import           Data.Functor.Identity ( Identity )

import Data.Char
    ( isSpace
    )

import           Text.Parsec
    ( ParsecT
    , oneOf
    )

type Parser = ParsecT String () Identity

-- | not efficient.
trim :: String -> String
trim = reverse . trim' . reverse . trim' where
    trim' "" = ""
    trim' (x:xs)
      | isSpace x = trim xs
      | otherwise = x:xs

-- sp :: Parser Char
-- sp = oneOf " \t\n"

