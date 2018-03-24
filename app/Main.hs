{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Text.Printf ( printf )
import           Data.Foldable ( foldl' )
import           Control.Applicative ( empty )
import           Data.ByteString as BS ( ByteString )
import           System.Directory ( getCurrentDirectory )
import           Data.Monoid ( (<>) )
import           Control.Monad ( join )
import qualified Text.RawString.QQ as QQ ( r )

import           Data.Vector as Dvec ( take )

import           System.FilePath.Glob as Sfg ( globDir1 )
import qualified System.FilePath.Glob as Sfg ( compile )

import           Codec.MeshObjGles.Types ( Config (Config)
                                         , Sequence (Sequence)
                                         , SequenceFrame (SequenceFrame)
                                         , Burst (Burst)
                                         , Vertices
                                         , TexCoords
                                         , Normals
                                         )
import           Codec.MeshObjGles.Parse ( parse )

binDir = getCurrentDirectory
framesDir = (<> "/example/wolf/frames-wait/") <$> binDir
textureDir = (<> "/example/wolf/textures/") <$> binDir

objFilenameGlob = "wolf_00010*.obj"
mtlFilename = (<> "/wolf_000100.mtl") <$> framesDir

main :: IO ()
main = do
    framesDir' <- framesDir
    textureDir' <- textureDir
    mtlFilename' <- mtlFilename
    objFilenames' <- sort <$> globDir1 (Sfg.compile objFilenameGlob) framesDir'
    print objFilenames'
    let config = Config textureDir' objFilenames' mtlFilename' textureConfigYaml
    p <- parse config
    printSeq p
    pure ()

printSeq sequ = do
    let Sequence frames = sequ
    mapM_ printFrame frames
    pure ()

printFrame frame = do
    putStrLn "• new frame"
    let SequenceFrame bursts = frame
    mapM_ printBurst bursts
    pure ()

printBurst burst = do
    putStrLn "•     new burst"
    let Burst vertices texCoordsMb normalsMb material = burst
        pref = "•        " :: String
    putStrLn $ printf "%svertices [first 3]: %s" pref (show . Dvec.take 3 $ vertices)
    putStrLn $ printf "%stexCoords [first 3]: %s" pref (show $ Dvec.take 3 <$> texCoordsMb)
    putStrLn $ printf "%snormals [first 3]: %s" pref (show $ Dvec.take 3 <$> normalsMb)
    printMaterial pref material
    pure ()

printMaterial pref material = do
    putStrLn $ printf "%smaterial: %s" pref (show material)
    pure ()

-- mappings are a little bit random and based a bit on guess-work (e.g. fur
-- (fella) for teeth).
-- Kd: diffuse texture map
-- Ka: alpha texture map
-- Ke: emissive texture map
textureConfigYaml :: ByteString
textureConfigYaml = [QQ.r|
textures:
  - materialName: Material
    image: body.png.base64
    width: 4096
    height: 2048
  - materialName: eyes
    image: eyes-2.png.base64
    width: 256
    height: 256
  - materialName: fur
    image: fur.png.base64
    width: 256
    height: 256
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






















