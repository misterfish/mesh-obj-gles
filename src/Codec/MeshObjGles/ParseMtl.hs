{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.MeshObjGles.ParseMtl ( parse
                , start ) where

import           Data.Text as Dtext ( Text, pack, unpack, intercalate, splitOn )
import           Data.Foldable ( foldl' )
import           Data.Function ( (&) )
import           Data.Functor.Identity ( Identity )
import           Debug.Trace ( trace )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad ( (<=<), void )
import           Control.Applicative ( (<|>) )
import           Data.Monoid ( (<>) )

import           Control.Monad.Trans.Except ( ExceptT, runExceptT )

import           Control.Monad.IO.Class ( liftIO )
import           Data.Vector as DV ( Vector
                                   , snoc )
import qualified Data.Vector as DV ( (++)
                                   , fromList
                                   , empty
                                   )

import           Data.Set as Dset ( empty, fromList )
import           Data.Map as Dmap ( Map )
import qualified Data.Map as Dmap ( empty
                                  , insert
                                  , fromList
                                  , lookup )

import           Text.Printf ( printf )
import           Text.Parsec ( char
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
                             , oneOf
                             , sepBy
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

import Codec.MeshObjGles.ParseUtil ( trim
                                   , fmapLeftT
                                   , hoistIOExcept
                                   , sepBy1X )
import Codec.MeshObjGles.Types
             ( Parser
             , MaterialMapMaterial
             , TextureTypesSet
             , TextureType (TextureDiffuse, TextureAmbient, TextureDissolve, TextureSpecular, TextureSpecularExp, TextureEmissive)
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

parse :: String -> TextureMap -> ExceptT String IO MaterialMapMaterial
parse input textureMap = do
    let p :: ExceptT ParseError IO MaterialMapMaterial
        p = parseInput (start textureMap) () input
        show' x = "bad parse: " <> show x
    fmapLeftT show' p

parseInput :: Parser a -> () -> String -> ExceptT ParseError IO a
parseInput start' initState = hoistIOExcept . runParserT start' initState "(no filename)" . trim

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
    option "" $ string "Ke" *> many1 notNl <* nl
    option "" $ string "Ni" *> many1 notNl <* nl
    option "" $ string "d" *> many1 notNl <* nl
    option "" $ string "illum" *> many1 notNl
    tt <- textureTypes
    -- liftIO . putStrLn $ printf "name: %s, tt: %s" name (show tt)
    let texMb = lookupTexture textureMap name
    manyTill anyToken sepOrEof
    pure $ Material (Dtext.pack name) se ac dc sc tt texMb where

textureTypes :: Parser TextureTypesSet
textureTypes = try textureTypes' <|> pure []

textureTypes' :: Parser TextureTypesSet
textureTypes' = do
    nl
    pure . Dset.fromList =<< (textureType `sepBy1X` nl)

textureType :: Parser TextureType
textureType  =  try textureTypeSpecularExp
            <|> try textureTypeDiffuse
            <|> try textureTypeAmbient
            <|> try textureTypeEmissive
            <|> try textureTypeSpecular
            <|> try textureTypeDissolve

textureTypeDissolve :: Parser TextureType
textureTypeDissolve = string "map_d " *> many1 notNl *> pure TextureDissolve
textureTypeSpecular :: Parser TextureType
textureTypeSpecular = string "map_Ks " *> many1 notNl *> pure TextureSpecular
textureTypeEmissive :: Parser TextureType
textureTypeEmissive = string "map_Ke " *> many1 notNl *> pure TextureEmissive
textureTypeAmbient  :: Parser TextureType
textureTypeAmbient  = string "map_Ka " *> many1 notNl *> pure TextureAmbient
textureTypeDiffuse  :: Parser TextureType
textureTypeDiffuse  = string "map_Kd " *> many1 notNl *> pure TextureDiffuse
textureTypeSpecularExp :: Parser TextureType
textureTypeSpecularExp = string "map_Ns " *> many1 notNl *> pure TextureSpecularExp

lookupTexture :: TextureMap -> String -> Maybe Texture
lookupTexture textureMap textureName = do
    let textureNameTxt = Dtext.pack textureName
    textureConfig' <- textureMap & Dmap.lookup textureNameTxt
    let width' = tcWidth textureConfig'
        height' = tcHeight textureConfig'
        tcImageBase64' = tcImageBase64 textureConfig'
    pure $ Texture tcImageBase64' width' height'

sepOrEof = try (void sep') <|> try eof where
    sep' = try . lookAhead $ string "\n\n"
    -- sep' = try . lookAhead $ string "@"

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

-- not efficient: to Text and back.
replaceStr wat met = Dtext.unpack . Dtext.intercalate met . Dtext.splitOn wat . Dtext.pack

