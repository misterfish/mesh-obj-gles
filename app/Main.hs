{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Data.ByteString as BS ( ByteString )
import           System.Directory ( getCurrentDirectory )
import           Data.Monoid ( (<>) )
import           Control.Monad ( join )
import           Text.RawString.QQ ( r )

import           Codec.MeshObjGles.Types ( Config (Config) )
import           Codec.MeshObjGles.Parse ( parse )

binDir = getCurrentDirectory
framesDir = (<> "/example/wolf/frames-wait/") <$> binDir
textureDir = (<> "/example/wolf/textures/") <$> binDir
objFilename = "wolf_000001.obj"
mtlFilename = "wolf_000001.mtl"
-- objFilename = "test_000001.obj"

main :: IO ()
main = do
    framesDir' <- framesDir
    textureDir' <- textureDir
    let config = Config framesDir' textureDir' objFilename mtlFilename textureConfigYaml
    p <- parse config
    print p

textureConfigYaml :: ByteString
textureConfigYaml = [r|
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

