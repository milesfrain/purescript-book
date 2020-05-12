module Test.Calculate where

import Prelude

import Control.Promise (Promise)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Uncurried (EffectFn2)

foreign import square :: Number -> Number


foreign import diagonal :: Fn2 Number Number Number

foreign import diagonalNested :: Number -> Number -> Number
foreign import diagonalArrow  :: Number -> Number -> Number


uncurriedAdd :: Fn2 Int Int Int
uncurriedAdd = mkFn2 \n m -> m + n

curriedAdd :: Int -> Int -> Int
curriedAdd n m = m + n


foreign import cumulativeSums :: Array Int -> Array Int

type Complex = {
  real :: Number,
  imag :: Number
}

foreign import addComplex :: Complex -> Complex -> Complex

foreign import maybeHeadImpl :: forall a. (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Array a -> Maybe a

maybeHead :: forall a. Array a -> Maybe a
maybeHead arr = maybeHeadImpl Just Nothing arr


foreign import data Undefined :: Type -> Type

foreign import undefinedHead :: forall a. Array a -> Undefined a

foreign import isUndefined :: forall a. Undefined a -> Boolean

isEmpty :: forall a. Array a -> Boolean
isEmpty = isUndefined <<< undefinedHead


foreign import unsafeHead :: forall a. Array a -> a



foreign import quadraticRootsImpl :: (forall a. a -> a -> Pair a) -> Number -> Number -> Number -> Pair Complex

quadraticRoots :: Number -> Number -> Number -> Pair Complex
quadraticRoots a b c = quadraticRootsImpl Pair a b c


foreign import myArr :: Array Int -> Array Int


foreign import diagonalLog :: EffectFn2 Number Number Number

foreign import sleep :: Int -> Promise Unit

foreign import diagonalAsync :: Number -> Number -> Promise Number

foreign import diagonalAsyncEffect :: Number -> Number -> Effect (Promise Number)

type QuadRec = {a :: Number, b :: Number, c :: Number}

foreign import showQuadRec :: QuadRec -> Int

foreign import sh :: forall a. a -> Int

foreign import cumulativeSumsBroken :: Array Int -> Array Int

foreign import addComplexBroken :: Complex -> Complex -> Complex

foreign import cumulativeSumsJson :: Array Int -> Json

cumulativeSumsDecoded :: Array Int ->  Either String (Array Int)
cumulativeSumsDecoded arr = decodeJson $ cumulativeSumsJson arr

foreign import addComplexJson :: Complex -> Complex -> Json

addComplexDecoded :: Complex -> Complex ->  Either String Complex
addComplexDecoded a b = decodeJson $ addComplexJson a b

foreign import mapSetFooJson :: Json -> Json

mapSetFoo :: Map String Int -> Either String (Map String Int)
mapSetFoo m = decodeJson $ mapSetFooJson $ encodeJson m

foreign import valuesOfMapJson :: Json -> Json

valuesOfMap :: Map String Int -> Either String (Set Int)
valuesOfMap m = decodeJson $ valuesOfMapJson $ encodeJson m

valuesOfMapGeneric :: forall k v.
  EncodeJson k =>
  EncodeJson v =>
  DecodeJson v =>
  Ord k =>
  Ord v =>
  Map k v ->
  Either String (Set v)
valuesOfMapGeneric m = decodeJson $ valuesOfMapJson $ encodeJson m