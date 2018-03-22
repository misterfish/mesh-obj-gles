module Codec.MeshObjGles.Parse ( parseMtl
             , parseObj
             , startMtl
             , startObj ) where

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
import Codec.MeshObjGles.Types ( SMaterial (SMaterial)
             , Material (Material)
             , Vertex2 (Vertex2)
             , Vertex3 (Vertex3)
             , ParserState (ParserStateMtl, ParserStateObj)
             , Coord (PosVertex, TexCoord, Normal)
             , materialName
             , materialSpecularExp
             , materialAmbientColor
             , materialDiffuseColor
             , materialSpecularColor
             , stPosVertices
             , stTexCoords
             , stNormals
             )

-- runParserT: generic parser with arbitrary state over arbitrary monad.

type Parser a = ParsecT String ParserState IO a

parseMtl :: String -> IO SMaterial
parseMtl s = either' <$> parseInput startMtl ParserStateMtl s where
    either' = either error' id
    error' x = error $ "bad parse: " <> show x

parseObj :: String -> IO SObj
parseObj s = either' <$> parseInput startObj (ParserStateObj DV.empty DV.empty DV.empty) s where
    either' = either error' id
    error' x = error $ "bad parse: " <> show x

parseInput :: Parser a -> ParserState -> String -> IO (Either ParseError a)
parseInput start initState = runParserT start initState "(no filename)" . trim

data SObj = SObj
    deriving Show

startObj :: Parser SObj
startObj = do
    -- optional comments
    -- many spnl
    -- string "mtllib" *> line

    -- xxx
    line -- o

    cs <- coord `sepBy` nl
    st <- getState
    liftIO . putStrLn $ printf "before: %s %s" (show st) (show cs)
    -- updateStatePosVertices vs
    -- updateStateTexCoords ts

    pure SObj

updateState :: (a -> ParserState -> ParserState) -> a -> Parser ()
updateState updater xs = do
    st <- getState
    liftIO . putStrLn $ printf "setting to st: %s" (show st)
    a <- setState $ updater xs st
    pure ()

coord :: Parser Coord
coord = try vertex' <|> try texCoord' where
    vertex' = PosVertex <$> getVertex3 "v"
    texCoord' = TexCoord <$> getVertex2 "vt"

startMtl :: Parser SMaterial
startMtl = do
    optional comments
    many spnl
    ms <- material `sepBy` (nl >> nl)
    let fold' acc m = acc & DM.insert (materialName m) m
    pure . SMaterial $ foldl' fold' DM.empty ms

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
    pure $ Material name se ac dc sc where

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

updateStatePosVertices = updateState updateStatePosVertices' where
    updateStatePosVertices' vs st = st { stPosVertices = st & stPosVertices
                                                            & vadd vs }

updateStateTexCoords = updateState updateStateTexCoords' where
    updateStateTexCoords'   ts st  = st { stTexCoords   = st & stTexCoords
                                                             & vadd ts }

updateStateNormals = updateState updateStateNormals' where
    updateStateNormals'     ns st  = st { stNormals     = st & stNormals
                                                             & vadd ns }

vadd xs vec = vec DV.++ DV.fromList xs

