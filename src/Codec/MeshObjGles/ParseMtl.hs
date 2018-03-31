module Codec.MeshObjGles.ParseMtl ( parse
                , start ) where

import           Data.Text as T ( Text, pack )
import           Data.Foldable ( foldl' )
import           Data.Function ( (&) )
import           Data.Functor.Identity ( Identity )
import           Debug.Trace ( trace )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad ( (<=<), void )
import           Data.Monoid ( (<>) )
import           Control.Monad.Trans.Either ( EitherT, runEitherT, hoistEither )
import           Control.Monad.IO.Class ( liftIO )
import           Data.Vector as DV ( Vector
                                   , snoc )
import qualified Data.Vector as DV ( (++)
                                   , fromList
                                   , empty
                                   )

import           Data.Map as Dmap ( Map )
import qualified Data.Map as Dmap ( empty
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

import Codec.MeshObjGles.ParseUtil ( trim, fmapLeftT, hoistIOEither )
import Codec.MeshObjGles.Types
             ( MaterialMapMaterial
             , Material (Material)
             , Vertex2 (Vertex2)
             , Vertex3 (Vertex3)
             , TextureMap
             , Texture (Texture)
             , tcWidth
             , tcHeight
             , tcImageBase64
             , materialName
             , materialSpecularExp
             , materialAmbientColor
             , materialDiffuseColor
             , materialSpecularColor )

-- runParserT: generic parser with arbitrary state over arbitrary monad.

type Parser a = ParsecT String () IO a

parse :: String -> TextureMap -> EitherT String IO MaterialMapMaterial
parse str textureMap = do
    let p :: EitherT ParseError IO MaterialMapMaterial
        p = parseInput (start textureMap) () str
        show' x = "bad parse: " <> show x
    fmapLeftT show' p

parseInput :: Parser a -> () -> String -> EitherT ParseError IO a
parseInput start' initState = hoistIOEither . runParserT start' initState "(no filename)" . trim

start :: TextureMap -> Parser MaterialMapMaterial
start textureMap = do
    optional comments
    many spnl
    ms <- (material textureMap) `sepBy` (nl >> nl)
    let fold' acc m = acc & Dmap.insert (materialName m) m
    pure $ foldl' fold' Dmap.empty ms

comments = comment `endBy` nl
comment = char '#' >> ( many $ noneOf "\n" )

nl = char '\n'

material :: TextureMap -> Parser Material
material textureMap = do
    name <- string "newmtl" *> many1 sp *> many1 notNl <* nl
    se <- string "Ns " *> float <* nl
    ac <- getVertex3 "Ka" <* nl
    dc <- getVertex3 "Kd" <* nl
    sc <- getVertex3 "Ks" <* nl
    let texMb = lookupTexture textureMap name
    manyTill anyToken sepOrEof
    pure $ Material (T.pack name) se ac dc sc texMb where

lookupTexture :: TextureMap -> String -> Maybe Texture
lookupTexture textureMap textureName = do
    let textureNameTxt = T.pack textureName
    textureConfig' <- textureMap & Dmap.lookup textureNameTxt
    let width' = tcWidth textureConfig'
        height' = tcHeight textureConfig'
        tcImageBase64' = tcImageBase64 textureConfig'
    pure $ Texture tcImageBase64' width' height'

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

