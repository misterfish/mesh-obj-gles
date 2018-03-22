{-# LANGUAGE OverloadedStrings #-}

-- | The root structure is a Sequence, which is a mashup of several .obj
-- files, one .mtl file, and one (for now non-optional) texture per .obj.
--
-- A Sequence contains a list of Obj structures.
--
-- An Obj is a coupling of a Texture and a list of Burst structures.
--
-- Texture info has to be provided separately through a yaml-style
-- configuration. Blender doesn't export it so you have to figure out the
-- mapping manually. The image must be a .png and already be in base64
-- format.
--
-- A Burst is a coupling of Vertices, optional TexCoords, optional Normals,
-- and a Material. An object (in the Blender sense -- e.g. Cube.002)
-- is rendered by painting one or more Bursts. A Blender object needs as
-- many Bursts as it has materials, where new materials are signified in the
-- .obj file with the 'usemtl' directive.
--
-- We assume that every Blender object uses at least one material. Also,
-- Blender exports one .mtl file for each .obj file, but we assume they are
-- all the same, so we only parse the first one).
--
-- Parsing an .mtl file yields MaterialMapMaterial, which is a map of Text
-- (material name) to Material structure.
--
-- The .mtl contains path names to images but these should be ignored.
--
-- Parsing an .obj file yields a map of maps, first mapping an object name
-- (e.g. Cube.002) to a MaterialMapCoords, then mapping on texture name
-- (e.g. Fur) to yield a triplet of (Maybe) Vertices, TexCoords, and
-- Normals.
--
-- (For symmetry Vertices is also a Maybe, but it is not optional).
--
-- On the GL side, each Burst corresponds to one drawArrays call. Before
-- starting the next Burst, the new material properties need to be sent as
-- attributes and/or uniforms. You can also optionally 'use' a new shader
-- program if desired, but be sure to send the same MVP matrix uniforms as
-- the current program. If the next Burst also starts a new Object, then the
-- active texture needs to be switched (e.g. our `Coords.activeTexture`
-- routine) as well.

-- A sequence (e.g. walk sequence, run sequence) is a list of .obj files. At
-- some point perhaps it might be good to allow .obj files to be shared if
-- they are common to several sequences, but for now they are all parsed
-- separately.

module Codec.MeshObjGles.Types ( Config (Config)
                               , MaterialMapCoords
                               , ObjMap
                               , MaterialMapMaterial
                               , Burst (Burst)
                               , Coords
                               , Material (Material)
                               , Obj (Obj)
                               , ObjName
                               , MtlName
                               , Texture (Texture)
                               , Vertex3 (Vertex3)
                               , Vertex2 (Vertex2)
                               , TextureConfigI (TextureConfigI)
                               , TextureConfigIT (TextureConfigIT)
                               , TextureConfig (TextureConfig)
                               , TextureMap
                               , Sequence (Sequence)
                               , configFramesDir
                               , configTextureDir
                               , configObjFilename
                               , configMtlFilename
                               , configTextureConfigYaml
                               , materialName
                               , materialSpecularExp
                               , materialAmbientColor
                               , materialDiffuseColor
                               , materialSpecularColor
                               , tciImagePath
                               , tciWidth
                               , tciHeight
                               , tciObjectName
                               , tcImageBase64
                               , tcWidth
                               , tcHeight
                               ) where

import           Data.ByteString ( ByteString )
import           Data.Text ( Text )
import           Data.Map as DM ( Map )
import qualified Data.Map as DM ( empty
                                , insert
                                , fromList
                                , lookup )
import           Data.Vector ( Vector )

import           Data.Yaml as Y ( (.:)
                                , FromJSON
                                , parseJSON
                                )
import qualified Data.Yaml as Y ( Value (Object)
                                , Parser )

data Config = Config { configFramesDir :: FilePath
                     , configTextureDir :: FilePath
                     , configObjFilename :: FilePath
                     , configMtlFilename :: FilePath
                     , configTextureConfigYaml :: ByteString }

data Sequence  = Sequence [Obj] deriving Show

data Obj       = Obj Texture [Burst] deriving Show

type MaterialMapMaterial = Map MtlName Material

data Material = Material { materialName :: MtlName
                         , materialSpecularExp :: Float
                         , materialAmbientColor :: Vertex3
                         , materialDiffuseColor :: Vertex3
                         , materialSpecularColor :: Vertex3 }
                         deriving Show

type PngBase64 = ByteString
data Texture   = Texture PngBase64 Int Int
    deriving Show
data Burst     = Burst Vertices (Maybe TexCoords) (Maybe Normals) Material
    deriving Show

type Vertices  = Vector Vertex3
type TexCoords = Vector Vertex2
type Normals   = Vector Vertex3

type ObjName = Text
type MtlName = Text
type ObjMap  = Map ObjName MaterialMapCoords
type MaterialMapCoords = Map MtlName Coords

type Coords  = (Maybe Vertices, Maybe TexCoords, Maybe Normals)

data Vertex2 = Vertex2 Float Float deriving Show
data Vertex3 = Vertex3 Float Float Float deriving Show
data Vertex4 = Vertex4 Float Float Float Float deriving Show

type TextureMap = Map ObjName TextureConfig

data TextureConfigI = TextureConfigI [TextureConfigIT] deriving Show

data TextureConfigIT = TextureConfigIT { tciImagePath :: Text
                                       , tciWidth :: Int
                                       , tciHeight :: Int
                                       , tciObjectName :: ObjName }
                                       deriving Show

-- like TextureConfigIT but with the image loaded & encoded.
data TextureConfig = TextureConfig { tcImageBase64 :: ByteString
                                   , tcWidth :: Int
                                   , tcHeight :: Int }

instance FromJSON TextureConfigI where
    parseJSON (Y.Object v) = do
        TextureConfigI <$> v .: "textures"
    parseJSON _ = error "invalid type for parseJSON TextureConfigI"

instance FromJSON TextureConfigIT where
    parseJSON (Y.Object v) = do
        TextureConfigIT <$>
            v .: "image" <*>
            v .: "width" <*>
            v .: "height" <*>
            v .: "objectName"
    parseJSON _ = error "invalid type for parseJSON TextureConfigIT"

-- makeInfiniteSequence :: Sequence -> Sequence
-- makeInfiniteSequence (Sequence s) = Sequence $ g s where
--     g = concat . repeat
--
-- tailSequence :: Sequence -> Sequence
-- tailSequence (Sequence s) = Sequence $ tail s
