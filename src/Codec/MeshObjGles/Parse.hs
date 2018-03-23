{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Codec.MeshObjGles.Parse ( Config (Config)
                               , TextureConfig (TextureConfig)
                               , Sequence (Sequence)
                               , SequenceFrame (SequenceFrame)
                               , Obj (Obj)
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
                               , makeInfiniteSequence
                               , tailSequence
                               , parse ) where

import           Data.ByteString as BS ( ByteString )
import qualified Data.ByteString as BS ( readFile )
import           Data.Text as T ( Text )
import qualified Data.Text as T ( pack, unpack )
import           Data.Function ( (&) )
import           Data.Foldable ( foldl' )
import           Data.List ( groupBy )
import           Data.Maybe ( fromJust, isJust )
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
                                , decode
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
                                 , fromFile
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

import           Codec.MeshObjGles.ParseUtil ( frint
                                             , verple2
                                             , verple3
                                             , verl2
                                             , verl3
                                             , dec
                                             , asterisk
                                             , fst3
                                             , snd3
                                             , thd3 )

import           Codec.MeshObjGles.Types ( Config (Config)
                                         , Coords
                                         , Sequence (Sequence)
                                         , SequenceFrame (SequenceFrame)
                                         , ObjName
                                         , MtlName
                                         , Texture (Texture)
                                         , TextureConfigI (TextureConfigI)
                                         , TextureConfigIT
                                         , TextureConfig (TextureConfig)
                                         , TextureMap
                                         , Obj (Obj)
                                         , Burst (Burst)
                                         , MaterialMapCoords
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
                                         , configTextureDir
                                         , configObjFilenames
                                         , configMtlFilename
                                         , configTextureConfigYaml
                                         , materialName
                                         , materialSpecularExp
                                         , materialAmbientColor
                                         , materialDiffuseColor
                                         , materialSpecularColor
                                         , tcWidth
                                         , tcHeight
                                         , tcImageBase64
                                         , tciImagePath
                                         , tciWidth
                                         , tciHeight
                                         , tciObjectName )

import qualified Codec.MeshObjGles.ParseMtl as Pmtl  ( parse
                                                     , start )

import           Prelude hiding ( elem )

parse :: Config -> IO Sequence
parse config = do
    let Config textureDir objFilenames mtlFilename textureConfigYaml = config
    materialMapMaterial <- getMaterial mtlFilename
    frames <- flip mapM objFilenames $ \objFilename ->
        parseFrame' textureDir materialMapMaterial textureConfigYaml objFilename
    pure $ Sequence frames

parseFrame' textureDir materialMapMaterial textureConfigYaml objFilename = do
    objMap <- getObjMap objFilename
    textureMap <- getTextureMap (T.pack textureDir) textureConfigYaml

    let objNames = Dmap.keys objMap
        objNamesTex = Dmap.keys textureMap

    objs <- mapListM (makeObj textureMap materialMapMaterial) objMap
    pure $ SequenceFrame objs

makeObj :: TextureMap -> MaterialMapMaterial -> ObjName -> MaterialMapCoords -> IO Obj
makeObj textureMap mtlMapMaterial objName mtlMapCoords = do
    let bursts' = prepareBursts mtlMapMaterial mtlMapCoords
        textureConfig' = textureMap & fromJust . Dmap.lookup objName
    let width' = tcWidth textureConfig'
        height' = tcHeight textureConfig'
        tcImageBase64' = tcImageBase64 textureConfig'
        texture' = Texture tcImageBase64' width' height'
    pure $ Obj texture' bursts'

mapList :: Ord a => (a -> b -> c) -> Map a b -> [c]
mapList f m = map map' $ Dmap.keys m where
    map' key = f key $ val' key
    val' key = fromJust $ Dmap.lookup key m

mapListM :: (Ord a, Monad m) => (a -> b -> m c) -> Map a b -> m [c]
mapListM f m = mapM map' $ Dmap.keys m where
    map' key = f key $ val' key
    val' key = fromJust $ Dmap.lookup key m

getObjMap :: FilePath -> IO ObjMap
getObjMap objFilename = prepare' <$> parseObj objFilename where
    prepare' = either error' prepareFrame
    error' = error . printf "bad parse: %s"

parseObj :: FilePath -> IO (Either String WavefrontOBJ)
parseObj objFilename = do
    let file = objFilename
    parseFile file

getMaterial :: FilePath -> IO MaterialMapMaterial
getMaterial mtlFilename = do
    mtl <- readFile mtlFilename
    Pmtl.parse mtl :: IO MaterialMapMaterial

getTextureMap :: Text -> ByteString -> IO TextureMap
getTextureMap textureDir textureConfigYaml = maybe err' (prepareTextureConfig textureDir) textureConfig' where
    err' = error "Couldn't decode texture config yaml"
    textureConfig' = Y.decode textureConfigYaml

prepareBursts :: MaterialMapMaterial -> MaterialMapCoords -> [Burst]
prepareBursts materialMapMaterial materialMapCoords = objToBurst' materialMapCoords where
    objToBurst' mmc = map (toBurst materialMapMaterial) (objList' mmc)
    objList' mmc = Dmap.toList mmc

toBurst :: MaterialMapMaterial -> (MtlName, Coords) -> Burst
toBurst materialMap (mtlName, coords) = burst' where
    burst' = Burst vertices' (snd3 coords) (thd3 coords) material'
    vertices' = toPosition' $ fst3 coords
    -- achtung
    material' = fromJust $ Dmap.lookup mtlName materialMap
    toPosition' = maybe (error' "position") id
    error' = error . (<>) "Missing vertex info for "

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

parseFile = fromFile

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

prepareTextureConfig :: Text -> TextureConfigI -> IO TextureMap
prepareTextureConfig textureDir' (TextureConfigI tcs) = foldM prepare' Dmap.empty tcs where
    prepare' acc tci = do
        let imagePath' = (<> tciImagePath tci) textureDir'
            width' = tciWidth tci
            height' = tciHeight tci
        base64' <- BS.readFile $ T.unpack imagePath'
        let tc = TextureConfig base64' width' height'
        pure $ acc & Dmap.insert (tciObjectName tci) tc
