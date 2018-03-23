{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Data.Foldable ( foldl' )
import           Control.Applicative ( empty )
import           Data.ByteString as BS ( ByteString )
import           System.Directory ( getCurrentDirectory )
import           Data.Monoid ( (<>) )
import           Control.Monad ( join )
import qualified Text.RawString.QQ as QQ ( r )

import           System.FilePath.Glob as Sfg ( globDir1 )
import qualified System.FilePath.Glob as Sfg ( compile )

import           Codec.MeshObjGles.Types ( Config (Config)
                                         , Sequence (Sequence)
                                         , SequenceFrame (SequenceFrame)
                                         , Obj (Obj)
                                         , Burst (Burst)
                                         , Vertices
                                         , TexCoords
                                         , Normals
                                         )
import           Codec.MeshObjGles.Parse ( parse )

binDir = getCurrentDirectory
framesDir = (<> "/example/wolf/frames-wait/") <$> binDir
textureDir = (<> "/example/wolf/textures/") <$> binDir

objFilenameGlob = "wolf*.obj"
mtlFilename = (<> "/wolf_000001.mtl") <$> framesDir

main :: IO ()
main = do
    framesDir' <- framesDir
    textureDir' <- textureDir
    mtlFilename' <- mtlFilename
    objFilenames' <- sort <$> globDir1 (Sfg.compile objFilenameGlob) framesDir'
    print objFilenames'
    let config = Config textureDir' objFilenames' mtlFilename' textureConfigYaml
    p <- parse config
    printCoords p
    -- print p
    pure ()

printCoords sequ = do
    let Sequence frames = sequ
    mapM_ printFrame frames
    pure ()

printFrame frame = do
    let SequenceFrame objs = frame
    mapM_ printObj objs
    pure ()

printObj obj = do
    let Obj text bursts = obj
    mapM_ printBurst bursts
    pure ()

printBurst burst = do
    let Burst vertices texCoordsMb normalsMb material = burst
    print vertices
    pure ()

textureConfigYaml :: ByteString
textureConfigYaml = [QQ.r|
# --- the objectName mappings are just guesses.
textures:
  - image: fur.png.base64
    width: 400
    height: 200
    objectName: Cube.001
  - image: body.png.base64
    width: 4096
    height: 2048
    objectName: Cube.002
  - image: eyes-1.png.base64
    width: 256
    height: 256
    objectName: Cube
  - image: eyes-2.png.base64
    width: 256
    height: 256
    objectName: Plane
|]

sort = quickSort

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort ss = sorted' where
    l :: Int
    l = length ss
    pivot = ss !! pivotIdx
    pivotIdx = floor $ frint l / 2
    (left, right, _) = foldl' (quickSort' pivotIdx pivot) ([], [], 0) ss
    sorted' = quickSort left <> [pivot] <> quickSort right

quickSort' pivotIdx pivot (l, r, i) x = (ll, rr, ii) where
    ii = i + 1
    (ll, rr)
      | i == pivotIdx  = (l,        r)
      | x <= pivot     = (l <> [x], r)
      | otherwise      = (l,        r <> [x])

frint = fromIntegral






















