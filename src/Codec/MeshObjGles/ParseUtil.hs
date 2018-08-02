{-# LANGUAGE ScopedTypeVariables #-}

module Codec.MeshObjGles.ParseUtil ( trim
                                   , frint
                                   , verple2
                                   , verple3
                                   , verl2
                                   , fmapLeft
                                   , fmapLeftT
                                   , bimapExceptT
                                   , hoistIOExcept
                                   , verl3
                                   , dec
                                   , asterisk
                                   , mapListEither
                                   , mapListM
                                   , mapList
                                   , sepBy1X
                                   , fst3
                                   , snd3
                                   , thd3 ) where

import           Control.Monad ( (<=<), void )
import           Control.Monad.Trans.Except ( ExceptT(ExceptT), mapExceptT, runExceptT )
import           Control.Monad.IO.Class ( liftIO )
import           Data.Functor.Identity ( Identity )
import           Data.Char ( isSpace )
import           Data.Maybe ( fromJust, isJust )
import           Data.Map as Dmap ( Map )
import qualified Data.Map as Dmap ( keys, lookup )
import           Text.Parsec ( ParsecT
                             , oneOf
                             , many
                             , try )

import           Codec.MeshObjGles.Types ( Vertex2 (Vertex2)
                                         , Vertex3 (Vertex3) )

-- | not efficient.
trim :: String -> String
trim = reverse . trim' . reverse . trim' where
    trim' "" = ""
    trim' (x:xs)
      | isSpace x = trim xs
      | otherwise = x:xs

frint = fromIntegral
verple2 (a, b) = Vertex2 a b
verple3 (a, b, c) = Vertex3 a b c
verl2 [a, b] = Vertex2 a b
verl3 [a, b, c] = Vertex3 a b c

dec :: Integral a => a -> a
dec = (+ (-1))

fs `asterisk` x = map map' fs where map' f = f x

fst3 (a, b, c) = a
snd3 (a, b, c) = b
thd3 (a, b, c) = c

fmapLeft :: (a -> c) -> Either a b -> Either c b
fmapLeft f (Left l) = Left $ f l
fmapLeft _ (Right r) = Right r

bimapExceptT :: forall a b c d m. Functor m => (a -> c) -> (b -> d) -> ExceptT a m b -> ExceptT c m d
bimapExceptT f g xt = bimap' . runExceptT $ xt where
    bimap' :: Functor m => m (Either a b) -> ExceptT c m d
    bimap' x = ExceptT $ bimap'' <$> x
    bimap'' :: Either a b -> Either c d
    bimap'' (Left l) = Left . f $ l
    bimap'' (Right r) = Right . g $ r

-- fmapLeftT :: Functor m => (a -> c) -> EitherT a m b -> EitherT c m b
-- fmapLeftT f = bimapEitherT f id
fmapLeftT :: Functor m => (a -> c) -> ExceptT a m b -> ExceptT c m b
fmapLeftT f = bimapExceptT f id

-- EitherT a IO is the MonadIO instance.
-- hoistIOEither :: forall a b. IO (Either a b) -> EitherT a IO b
-- hoistIOEither = hoistEither <=< liftIO

hoistIOExcept :: forall a b. IO (Either a b) -> ExceptT a IO b
hoistIOExcept = ExceptT

-- included to increase readability a bit.
mapListEither :: Ord a => (a -> b -> Either d c) -> Map a b -> Either d [c]
mapListEither = mapListM

mapList :: Ord a => (a -> b -> c) -> Map a b -> [c]
mapList = mapList' map

mapListM :: (Ord a, Monad m) => (a -> b -> m c) -> Map a b -> m [c]
mapListM = mapList' mapM

mapList' mapper f m = mapper map' $ Dmap.keys m where
    map' key = f key $ val' key
    val' key = fromJust $ Dmap.lookup key m

-- A variation on sepBy1 which backtracks if the input ends on the
-- separator.
-- To see why this is useful, consider the input: "1,2,3,4,,7,8,9", with the
-- desired result [[1,2,3,4], [7,8,9]]
-- If you use ordinary `sepBy` to break into chunks on ',,' and then within
-- the chunks on `,`, it will fail.
sepBy1X p sep = (:) <$> p <*> q where
    q = many . try $ sep >> p

