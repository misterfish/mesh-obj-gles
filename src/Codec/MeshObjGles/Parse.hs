{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Codec.MeshObjGles.Parse ( Config (Config)
                               , ConfigTextureSpec (ConfigTextureDir)
                               , ConfigObjectSpec (ConfigObjectSpec)
                               , ConfigObjectSpecItem (ConfigObjectFilePath, ConfigObjectSource)
                               , ConfigMtlSpec (ConfigMtlFilePath, ConfigMtlSource)
                               , TextureConfig (TextureConfig)
                               , Sequence (Sequence)
                               , SequenceFrame (SequenceFrame)
                               , Texture (Texture)
                               , Burst (Burst)
                               , Vertices
                               , TexCoords
                               , TextureMap
                               , ObjName
                               , MtlName
                               , Normals
                               , Vertex2 (Vertex2)
                               , Vertex3 (Vertex3)
                               , materialName
                               , materialSpecularExp
                               , materialAmbientColor
                               , materialDiffuseColor
                               , materialSpecularColor
                               , materialTexture
                               , makeInfiniteSequence
                               , tailSequence
                               , tcImageBase64
                               , tcHeight
                               , tcWidth
                               , parse ) where

import           Data.ByteString as BS ( ByteString )
import qualified Data.ByteString as BS ( readFile )
import qualified Data.ByteString.Char8 as BS8 ( pack, unpack )
import           Data.Text as T ( Text )
import qualified Data.Text as T ( pack, unpack )
import           Data.Function ( (&) )
import           Data.Foldable ( foldl' )
import           Data.List ( groupBy )
import           Data.Maybe ( fromJust, isJust )
import           Control.Monad.Trans.Either ( EitherT, runEitherT, hoistEither, bimapEitherT )
import           Control.Monad.IO.Class ( liftIO )
import           Data.Monoid ( (<>) )
import           Text.Printf ( printf )
import           Control.Monad ( (<=<)
                               , ap
                               , foldM
                               , void
                               , join )

import           Data.Vector as DVec ( Vector, (!) )
import qualified Data.Vector as Dvec ( (++)
                                     , map
                                     , snoc
                                     , toList
                                     , empty
                                     , fromList )

import           Text.RawString.QQ ( r )
import           Data.Yaml as Y ( (.:)
                                , FromJSON
                                , ParseException
                                , decodeEither'
                                , prettyPrintParseException
                                , parseJSON )
import qualified Data.Yaml as Y ( Value (Object)
                                , Parser )
import           Data.Map as Dmap ( Map )
import qualified Data.Map as Dmap ( empty
                                  , keys
                                  , toList
                                  , lookup
                                  , insert
                                  , unionWith
                                  , insertWith
                                  )

import           Codec.Wavefront ( WavefrontOBJ
                                 , Face (Face)
                                 , FaceIndex (FaceIndex)
                                 , Location
                                 , objFaces
                                 , objLocations
                                 , objTexCoords
                                 , objNormals
                                 , locX, locY, locZ
                                 , norX, norY, norZ
                                 , texcoordR, texcoordS
                                 , elObject
                                 , elValue
                                 , elMtl )

import qualified Codec.Wavefront as Cwav ( fromFile
                                         , fromSource )

import           Codec.MeshObjGles.ParseUtil ( frint
                                             , fmapLeftT
                                             , verple2
                                             , verple3
                                             , verl2
                                             , hoistIOEither
                                             , verl3
                                             , dec
                                             , asterisk
                                             , mapListEither
                                             , mapListM
                                             , mapList
                                             , fst3
                                             , snd3
                                             , thd3 )

import           Codec.MeshObjGles.Types ( Config (Config)
                                         , ConfigTextureSpec (ConfigTextureDir)
                                         , ConfigObjectSpec (ConfigObjectSpec)
                                         , ConfigObjectSpecItem (ConfigObjectFilePath, ConfigObjectSource)
                                         , ConfigMtlSpec (ConfigMtlFilePath, ConfigMtlSource)
                                         , Coords
                                         , Sequence (Sequence)
                                         , SequenceFrame (SequenceFrame)
                                         , ObjName
                                         , MtlName
                                         , Texture (Texture)
                                         , TextureConfigI (TextureConfigI)
                                         , TextureConfigIT
                                         , TextureConfig (TextureConfig)
                                         , TcitImageSpec (TcitImageFilePath, TcitImageBase64)
                                         , TextureMap
                                         , Burst (Burst)
                                         , MaterialMapCoordsI
                                         , MaterialMapMaterial
                                         , Material
                                         , ObjMap
                                         , Vertices
                                         , TexCoords
                                         , Normals
                                         , Vertex2 (Vertex2)
                                         , Vertex3 (Vertex3)
                                         , makeInfiniteSequence
                                         , tailSequence
                                         , configObjSpec
                                         , configMtlSpec
                                         , configTextureConfigYaml
                                         , materialName
                                         , materialSpecularExp
                                         , materialAmbientColor
                                         , materialDiffuseColor
                                         , materialSpecularColor
                                         , materialTexture
                                         , tcWidth
                                         , tcHeight
                                         , tcImageBase64
                                         , tciImageSpec
                                         , tciWidth
                                         , tciHeight
                                         , tciMaterialName )

import qualified Codec.MeshObjGles.ParseMtl as Pmtl  ( parse
                                                     , start )

import           Prelude hiding ( elem )

parse :: Config -> IO (Either String (Sequence, TextureMap))
parse = runEitherT . parse'

parse' :: Config -> EitherT String IO (Sequence, TextureMap)
parse' config = do
    let Config objSpec mtlSpec textureConfigYaml = config
        mtlSource
          | ConfigMtlFilePath fp' <- mtlSpec = BS.readFile fp'
          | ConfigMtlSource src' <- mtlSpec = pure src'
          | otherwise = error "ConfigMtlSpec"
    mtlSource' <- liftIO mtlSource
    textureMap <- getTextureMap textureConfigYaml
    let ConfigObjectSpec specItems' = objSpec
    materialMapMaterial <- getMaterialMap mtlSource' textureMap
    frames' <- flip mapM specItems' $ \item' ->
        parseFrame' materialMapMaterial textureConfigYaml item'
    pure (Sequence frames', textureMap)

parseFrame' :: MaterialMapMaterial -> ByteString -> ConfigObjectSpecItem -> EitherT String IO SequenceFrame
parseFrame' materialMapMaterial textureConfigYaml objSpecItem = do
    objMap <- getObjMap objSpecItem
    liftIO . putStrLn $ printf "objMap: %s" (show objMap)

    let objNames = Dmap.keys objMap
    bursts <- hoistEither . mapListEither (makeBursts materialMapMaterial) $ objMap
    pure . SequenceFrame $ concat bursts

-- Prepare list of Burst for a given blender object.
makeBursts :: MaterialMapMaterial -> ObjName -> MaterialMapCoordsI -> Either String [Burst]
makeBursts mtlMapMaterial objName mtlMapCoords =
    prepareBursts mtlMapMaterial mtlMapCoords

getObjMap :: ConfigObjectSpecItem -> EitherT String IO ObjMap
getObjMap = bimapEitherT error' prepareFrame . parseObj where
    error' = ("bad obj parse: " <>)

parseObj :: ConfigObjectSpecItem -> EitherT String IO WavefrontOBJ
parseObj (ConfigObjectFilePath fp) = hoistIOEither $ Cwav.fromFile fp
parseObj (ConfigObjectSource src)  = hoistIOEither $ Cwav.fromSource src

getMaterialMap :: ByteString -> TextureMap -> EitherT String IO MaterialMapMaterial
getMaterialMap mtlSource textureMap =
    Pmtl.parse (BS8.unpack mtlSource) textureMap

getTextureMap :: ByteString -> EitherT String IO TextureMap
getTextureMap textureConfigYaml = do
    let texConfigI :: EitherT String IO TextureConfigI
        texConfigI = fmapLeftT error' . hoistEither $ textureConfig'
        textureConfig' = Y.decodeEither' textureConfigYaml
        error' :: ParseException -> String
        error' = ("Couldn't decode texture config yaml: " <>) . prettyPrintParseException
    liftIO . prepareTextureConfig =<< texConfigI

prepareBursts :: MaterialMapMaterial -> MaterialMapCoordsI -> Either String [Burst]
prepareBursts materialMapMaterial materialMapCoords = objToBurst' materialMapCoords where
    objToBurst' mmc = mapM (toBurst materialMapMaterial) (objList' mmc)
    objList' mmc = Dmap.toList mmc

toBurst :: MaterialMapMaterial -> (MtlName, Coords) -> Either String Burst
toBurst materialMap (mtlName, coords) = do
    material' <- maybe error1' Right $ Dmap.lookup mtlName materialMap
    let toPosition' = maybe error2' Right
    vertices' <- toPosition' $ fst3 coords
--     putStrLn $ printf "toBurst: materialMap %s" (show materialMap)
    pure $ Burst vertices' (snd3 coords) (thd3 coords) material' where
        error1' = Left $ printf "Failed lookup for mtlName (%s) in materialMap (%s)" mtlName (show materialMap)
        error2' = Left "Missing vertex info for position"

prepareFrame :: WavefrontOBJ -> ObjMap
prepareFrame parsed = do
    let faces'  = objFaces parsed
    foldl' (foldElement parsed) Dmap.empty faces'

-- acc = { obj: { mtl: (Maybe Vector, Maybe Vector, Maybe Vector) } }
foldElement parsed acc elem = mapped' where
    mapped'       = Dmap.insertWith (Dmap.unionWith mergeMtl) obj' newmap' acc
    obj'          = fromJust . elObject $ elem
    mtl'          = fromJust . elMtl    $ elem
    face'         =            elValue  $ elem
    newmap'       = Dmap.empty & Dmap.insert mtl' newvec'
    newvec'       = toVertices parsed face'

-- merge each element of the triple.
-- merging two Justs leads to a new Just with the vectors appended;
-- merging anything else leads to Nothing.

mergeMtl :: Coords -> Coords -> Coords
mergeMtl (a, b, c) (x, y, z) = (merge' a x, merge' b y, merge' c z) where
    merge' n m = (<>) <$> n <*> m

groupByObj elem1 elem2 = a == b where
    a = elObject elem1
    b = elObject elem2

toVertices :: WavefrontOBJ -> Face -> (Maybe (Vector Vertex3), Maybe (Vector Vertex2), Maybe (Vector Vertex3))
toVertices parsed (Face v1 v2 v3 _extras) = vertices' where
    vertices' = foldl' fold' (vec', vec', vec') . map (toVertex parsed) $ [v1, v2, v3]
    vec' = Just Dvec.empty
    fold' (lvec, tvec, nvec) (lvermb, tvermb, nvermb) = ( lvec & vecAppendMaybe lvermb
                                                        , tvec & vecAppendMaybe tvermb
                                                        , nvec & vecAppendMaybe nvermb )

vecAppend = flip Dvec.snoc

vecAppendMaybe :: Maybe a -> Maybe (Vector a) -> Maybe (Vector a)
vecAppendMaybe a b = vecAppend <$> a <*> b

-- same, point-free.
vecAppendMaybe' :: Maybe a -> Maybe (Vector a) -> Maybe (Vector a)
vecAppendMaybe' = ap . fmap vecAppend

-- pos must be there
-- any missing tex coord or normal causes entire structure for this burst to
-- collapse to Nothing.
toVertex :: WavefrontOBJ -> FaceIndex -> (Maybe Vertex3, Maybe Vertex2, Maybe Vertex3)
toVertex parsed (FaceIndex locIndex texCoordIndexMb norIndexMb) = v where
    v = (l, t, n)
    l = Just . lookupLoc  $  locIndex
    t = lookupTexCoord   <$> texCoordIndexMb
    n = lookupNormal     <$> norIndexMb
    lookup' f        = (f parsed !) . dec
    lookupLoc x      = verl3 $ [locX, locY, locZ]     `asterisk` lookup' objLocations x
    lookupTexCoord x = verl2 $ [texcoordR, texcoordS] `asterisk` lookup' objTexCoords x
    lookupNormal x   = verl3 $ [norX, norY, norZ]     `asterisk` lookup' objNormals x

prepareTextureConfig :: TextureConfigI -> IO TextureMap
prepareTextureConfig (TextureConfigI tcs) = foldM prepare' Dmap.empty tcs where
    prepare' acc tci = do
        let imageSpec' = tciImageSpec tci
            base64IO'
              | TcitImageFilePath fp <- imageSpec' = slurp' fp
              | TcitImageBase64 b64 <- imageSpec' = pure $ BS8.pack b64
            slurp' = BS.readFile
            width' = tciWidth tci
            height' = tciHeight tci
        base64' <- base64IO'
        let tc = TextureConfig base64' width' height'
        pure $ acc & Dmap.insert (tciMaterialName tci) tc
