module Test.Calculate where

import Prelude

import Control.Promise (Promise)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
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


foreign import maybeHeadImpl :: forall a. (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Array a -> Maybe a

maybeHead :: forall a. Array a -> Maybe a
maybeHead arr = maybeHeadImpl Just Nothing arr


foreign import data Undefined :: Type -> Type

foreign import undefinedHead :: forall a. Array a -> Undefined a

foreign import isUndefined :: forall a. Undefined a -> Boolean

isEmpty :: forall a. Array a -> Boolean
isEmpty = isUndefined <<< undefinedHead


foreign import unsafeHead :: forall a. Array a -> a


-- todo Complex




foreign import diagonalLog :: EffectFn2 Number Number Number

foreign import sleep :: Int -> Promise Unit

foreign import diagonalAsync :: Number -> Number -> Promise Number

foreign import diagonalAsyncEffect :: Number -> Number -> Effect (Promise Number)

type QuadRec = {a :: Number, b :: Number, c :: Number}

foreign import showQuadRec :: QuadRec -> Int

foreign import sh :: forall a. a -> Int