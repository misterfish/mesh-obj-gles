{-# LANGUAGE OverloadedStrings #-}

-- | The root structure is a Sequence, which is a mashup of several .obj
-- files and one .mtl file.
--
-- A Sequence contains a list of SequenceFrame structures.
--
-- A SequenceFrame contains a list of Burst structures.
--
-- A Burst is a coupling of Vertices, optional TexCoords, optional Normals,
-- and a Material. An object (in the Blender sense -- e.g. Cube.002)
-- is rendered by painting one or more Bursts. A Blender object needs as
-- many Bursts as it has materials, where new materials are signified in the
-- .obj file with the 'usemtl' directive.
--
-- A blender object has no direct representation our parsed Sequence: we
-- only care about Burst structures.
--
-- We assume that every Blender object uses at least one material. Also,
-- Blender exports one .mtl file for each .obj file, but we assume they are
-- all the same, so we only parse the first one).
--
-- Parsing an .mtl file yields MaterialMapMaterial, which is a map of Text
-- (material name) to Material structure. A Material contains lighting info
-- and a Maybe Texture corresponding to the texture image.
--
-- The .mtl contains path names to texture images but these should be
-- ignored. (They could be the paths on the local machine of the blender
-- artist for example). The artist is responsible for making the texture
-- images available to us.
--
-- Texture info has to be provided separately through a yaml-style
-- configuration. Blender doesn't export it so you have to figure out the
-- mapping manually. The image must be a .png and already be in base64
-- format.
--
-- Parsing an .obj file yields a map of maps, first mapping an object name
-- (e.g. Cube.002) to a MaterialMapCoordsI, then mapping on texture name
-- (e.g. Fur) to yield a triplet of (Maybe) Vertices, TexCoords, and
-- Normals.
--
-- (For symmetry Vertices is also a Maybe, but it is not optional).
--
-- On the GL side, each Burst corresponds to one drawArrays call. Starting a
-- new Burst means activating a material and then uploading one to three
-- sets of vertices. The new material properties need to be sent as
-- attributes and/or uniforms. If the new material contains a texture then
-- the new texture also needs to get active (our `Coords.activeTexture`
-- routine). You can also optionally 'use' a new shader program if desired,
-- but be sure to send the same MVP matrix uniforms as the current program.

-- A sequence (e.g. walk sequence, run sequence) is a list of .obj files. At
-- some point perhaps it might be good to allow .obj files to be shared if
-- they are common to several sequences, but for now they are all parsed
-- separately.

module Codec.MeshObjGles.Types ( Config (Config)
                               , ConfigTextureSpec (ConfigTextureDir)
                               , ConfigObjectSpec (ConfigObjectSpec)
                               , ConfigObjectSpecItem (ConfigObjectFilePath, ConfigObjectSource)
                               , ConfigMtlSpec (ConfigMtlFilePath, ConfigMtlSource)
                               , MaterialMapCoordsI
                               , ObjMap
                               , MaterialMapMaterial
                               , Burst (Burst)
                               , Coords
                               , Material (Material)
                               , ObjName
                               , MtlName
                               , Vertices
                               , TexCoords
                               , Normals
                               , TextureMap
                               , Texture (Texture)
                               , Vertex3 (Vertex3)
                               , Vertex2 (Vertex2)
                               , TextureConfigI (TextureConfigI)
                               , TextureConfigIT (TextureConfigIT)
                               , TextureConfig (TextureConfig)
                               , TcitImageSpec (TcitImageFilePath, TcitImageBase64)
                               , Sequence (Sequence)
                               , SequenceFrame (SequenceFrame)
                               , configObjSpec
                               , configMtlSpec
                               , configTextureConfigYaml
                               , materialName
                               , materialSpecularExp
                               , materialAmbientColor
                               , materialDiffuseColor
                               , materialSpecularColor
                               , materialTexture
                               , tciImageSpec
                               , tciWidth
                               , tciHeight
                               , tciMaterialName
                               , tcImageBase64
                               , tcWidth
                               , tcHeight
                               , makeInfiniteSequence
                               , tailSequence
                               ) where

import           GHC.Generics ( Generic, Rep )
import           Text.Printf ( printf )
import           Data.ByteString as BS ( ByteString )
import qualified Data.ByteString as BS ( take, unpack )
import           Data.Text ( Text )
import           Data.Map as DM ( Map )
import qualified Data.Map as DM ( empty
                                , insert
                                , fromList
                                , lookup )
import           Data.Vector ( Vector )
import           Debug.Trace ( trace )
import           Data.Monoid ( (<>) )
import           Control.Applicative ( (<|>) )
import           Control.DeepSeq ( NFData
                                 , deepseq
                                 , rnf )
-- import           Control.DeepSeq.Generics ( genericRnf )
--                                  , GNFData )

import Control.Monad.IO.Class ( liftIO )
import           Data.Yaml as Y ( (.:)
                                , FromJSON
                                , parseJSON
                                )
import qualified Data.Yaml as Y ( Value (Object)
                                , Parser )

data Config = Config { configObjSpec :: ConfigObjectSpec
                     , configMtlSpec :: ConfigMtlSpec
                     , configTextureConfigYaml :: ByteString }

data ConfigTextureSpec = ConfigTextureDir FilePath
data ConfigObjectSpec = ConfigObjectSpec [ConfigObjectSpecItem]

data ConfigObjectSpecItem = ConfigObjectFilePath FilePath
                          | ConfigObjectSource Text

data ConfigMtlSpec = ConfigMtlFilePath FilePath
                   | ConfigMtlSource ByteString

data Sequence = Sequence ![SequenceFrame] deriving Show
data SequenceFrame  = SequenceFrame ![Burst] deriving Show

type MaterialMapMaterial = Map MtlName Material

type MaybeTexture = Maybe Texture

data Material = Material { materialName :: !MtlName
                         , materialSpecularExp :: !Float
                         , materialAmbientColor :: !Vertex3
                         , materialDiffuseColor :: !Vertex3
                         , materialSpecularColor :: !Vertex3
                         , materialTexture :: !MaybeTexture }
                         deriving Show

type PngBase64 = ByteString
data Texture   = Texture !PngBase64 !Int !Int

instance Show Texture where
    show (Texture pngBase64 width height) = printf "width: %d, height: %d, base64: %s [...]"
        width
        height
        (show . BS.take 10 $ pngBase64)

type MaybeTexCoords = Maybe TexCoords
type MaybeNormals = Maybe Normals

data Burst     = Burst !Vertices !MaybeTexCoords !MaybeNormals !Material
    deriving Show

type Vertices  = Vector Vertex3
type TexCoords = Vector Vertex2
type Normals   = Vector Vertex3

type ObjName = Text
type MtlName = Text
type ObjMap  = Map ObjName MaterialMapCoordsI
type MaterialMapCoordsI = Map MtlName Coords

type Coords  = (Maybe Vertices, Maybe TexCoords, Maybe Normals)

data Vertex2 = Vertex2 !Float !Float deriving Show
data Vertex3 = Vertex3 !Float !Float !Float deriving Show
data Vertex4 = Vertex4 !Float !Float !Float !Float deriving Show

type TextureMap = Map MtlName TextureConfig

data TextureConfigI = TextureConfigI ![TextureConfigIT] deriving Show

data TextureConfigIT = TextureConfigIT { tciMaterialName :: !MtlName
                                       , tciImageSpec :: !TcitImageSpec
                                       , tciWidth :: !Int
                                       , tciHeight :: !Int }
                                       deriving Show

data TcitImageSpec = TcitImageFilePath !FilePath
                   | TcitImageBase64 !String
                   | TcitDummy String -- silly way to show better error message

instance Show TcitImageSpec where
    show (TcitImageFilePath fp') = fp'
    show (TcitImageBase64 b64') = take 10 b64' <> " ..."

-- like TextureConfigIT but with the image loaded for sure.
data TextureConfig = TextureConfig { tcImageBase64 :: !ByteString
                                   , tcWidth :: !Int
                                   , tcHeight :: !Int }

instance FromJSON TextureConfigI where
    parseJSON (Y.Object v) = do
        TextureConfigI <$> v .: "textures"
    parseJSON _ = error "invalid type for parseJSON TextureConfigI"

instance FromJSON TextureConfigIT where
    parseJSON (Y.Object v) = do
        let trace' i = trace ("trace: " <> show i) i
        -- img <- (v .: "crimage") <|> (v .: "image") :: Y.Parser Friend
        -- _ <- pure . seq . trace' $ img
        -- liftIO . putStrLn $ printf "hello %s" (show img)
        TextureConfigIT <$>
            v .: "materialName" <*>
            (TcitImageFilePath <$> v .: "imageFile"
            <|> TcitImageBase64 <$> v .: "imageBase64"
            <|> TcitDummy <$> v .: "imageFile or imageBase64"
            ) <*>
            v .: "width" <*>
            v .: "height"
    parseJSON _ = error "invalid type for parseJSON TextureConfigIT"

makeInfiniteSequence :: Sequence -> Sequence
makeInfiniteSequence (Sequence s) = Sequence $ g s where
    g = concat . repeat

tailSequence :: Sequence -> Sequence
tailSequence (Sequence s) = Sequence $ tail s

-- Provide simple Generic/NFData instances so the consumer can force deep
-- evaluation with deepseq.

instance Generic Sequence
instance NFData Sequence where
    rnf (Sequence frames) = frames `deepseq` ()
instance Generic SequenceFrame
instance NFData SequenceFrame where
    rnf (SequenceFrame bursts) = bursts `deepseq` ()
instance Generic Burst
instance NFData Burst where
    rnf (Burst vertices texCoordsMb normalsMb material) =
        vertices `deepseq` texCoordsMb `deepseq` normalsMb `deepseq` material `deepseq` ()
instance Generic Vertex2
instance NFData Vertex2 where
    rnf (Vertex2 a b) = a `deepseq` b `deepseq` ()
instance Generic Vertex3
instance NFData Vertex3 where
    rnf (Vertex3 a b c) = a `deepseq` b `deepseq` c `deepseq` ()
instance Generic Material
instance NFData Material where
    rnf (Material n se ac dc sc mt) =
        n `deepseq` se `deepseq` ac `deepseq` dc `deepseq` sc `deepseq` mt `deepseq` ()
instance Generic Texture
instance NFData Texture where
    rnf (Texture png w h) = png `deepseq` w `deepseq` h `deepseq` ()
