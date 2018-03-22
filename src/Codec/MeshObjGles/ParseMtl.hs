module Codec.MeshObjGles.ParseMtl ( parse
                , start ) where

import           Data.Text as T ( pack )
import           Data.Foldable ( foldl' )
import           Data.Function ( (&) )
import           Data.Functor.Identity ( Identity )
import           Debug.Trace ( trace )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad ( (<=<), void )
import           Data.Monoid ( (<>) )
import           Data.Vector as DV ( Vector
                                   , snoc )
import qualified Data.Vector as DV ( (++)
                                   , fromList
                                   , empty
                                   )

import           Data.Map as DM ( Map )
import qualified Data.Map as DM ( empty
                                , insert
                                , fromList
                                , lookup )

import           Text.Printf ( printf )
import           Text.Parsec ( (<|>)
                             , char
                             , digit
                             , satisfy
                             , try
                             , string
                             , optional
                             , option
                             , many
                             , manyTill
                             , anyToken
                             , lookAhead
                             , many1
                             , oneOf , sepBy
                             , endBy
                             , sepBy1
                               -- can sometimes help resolve the trailing space problem.
                             , sepEndBy1
                             , Parsec
                             , ParsecT
                             , ParseError
                             , runParserT
                             , runParser
                             , getState
                             , setState
                             , eof
                             , noneOf
                             )

import qualified Text.ParserCombinators.Parsec as P ( parse )

import Codec.MeshObjGles.ParseUtil ( trim )
import Codec.MeshObjGles.Types ( MaterialMapCoords
             , MaterialMapMaterial
             , Material (Material)
             , Vertex2 (Vertex2)
             , Vertex3 (Vertex3)
             , materialName
             , materialSpecularExp
             , materialAmbientColor
             , materialDiffuseColor
             , materialSpecularColor
             )

-- runParserT: generic parser with arbitrary state over arbitrary monad.

type Parser a = ParsecT String () IO a

parse :: String -> IO MaterialMapMaterial
parse s = either' <$> parseInput start () s where
    either' = either error' id
    error' x = error $ "bad parse: " <> show x

parseInput :: Parser a -> () -> String -> IO (Either ParseError a)
parseInput start' initState = runParserT start' initState "(no filename)" . trim

start :: Parser MaterialMapMaterial
start = do
    optional comments
    many spnl
    ms <- material `sepBy` (nl >> nl)
    let fold' acc m = acc & DM.insert (materialName m) m
    pure $ foldl' fold' DM.empty ms

comments = comment `endBy` nl
comment = char '#' >> ( many $ noneOf "\n" )

nl = char '\n'
material = do
    name <- string "newmtl" *> many1 sp *> many1 notNl <* nl
    se <- string "Ns " *> float <* nl
    ac <- getVertex3 "Ka" <* nl
    dc <- getVertex3 "Kd" <* nl
    sc <- getVertex3 "Ks" <* nl
    manyTill anyToken sepOrEof
    pure $ Material (T.pack name) se ac dc sc where

sepOrEof = try (void sep') <|> try eof where
    sep' = try . lookAhead $ string "\n\n"

line = flip (:) <$> many1 notNl <*> nl

getVertex2 :: String -> Parser Vertex2
getVertex2 pref = do
    let str' = pref <> " "
        ac1' = string str' *> float <* sp
        ac2' = float
    Vertex2 <$> ac1' <*> ac2'

getVertex3 :: String -> Parser Vertex3
getVertex3 pref = do
    let str' = pref <> " "
        ac1' = string str' *> float <* sp
        ac2' = float <* sp
        ac3' = float
    Vertex3 <$> ac1' <*> ac2' <*> ac3'

float :: Parser Float
float = read' <$> ( try neg' <|> try pos' <|> try plain' ) where
    read' = read :: String -> Float
    neg' = (:) <$> char '-' <*> plain'
    pos' = (:) <$> char '+' <*> plain'
    plain' = (<>) <$> many1 digit <*> optdec'
    optdec' = option "" dec'
    dec' = (:) <$> char '.' <*> many1 digit

notNl = noneOf "\n"

int :: Parser Int
int = read' <$> (try plainint' <|> try posint' <|> try negint') where
    read' = read :: String -> Int
    posint' = char '+' *> plainint'
    negint' = (:) <$> char '-' <*> plainint'
    plainint' = many1 digit

spnl :: Parser Char
spnl = oneOf " \t\n"

sp :: Parser Char
sp = oneOf " \t"

vadd xs vec = vec DV.++ DV.fromList xs

