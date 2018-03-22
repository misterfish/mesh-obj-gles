{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           System.Directory ( getCurrentDirectory )
import           Data.Monoid ( (<>) )
import           Control.Monad ( join )

import           Codec.MeshObjGles.Parse ( parse )

binDir = getCurrentDirectory
framesDir = (<> "/example/wolf/frames-wait/") <$> binDir
textureDir = (<> "/example/wolf/textures/") <$> binDir

main :: IO ()
main = do
    framesDir' <- framesDir
    textureDir' <- textureDir
    p <- parse (framesDir', textureDir')
    print p
