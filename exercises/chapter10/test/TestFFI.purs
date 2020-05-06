module Test.TestFFI where

import Prelude

import Data.Foldable (class Foldable, product)
import Effect (Effect)

foreign import runGcd :: Int -> Int -> Int
foreign import runShout :: String

foreign import boldImpl :: forall a. (a -> String) -> a -> String

boldWrap :: forall a. Show a => a -> String
boldWrap x = boldImpl show x

foreign import boldConstraint :: forall a. Show a => a -> String

foreign import showEquality :: forall a. Eq a => Show a => a -> a -> String

foreign import yell :: forall a. Show a => a -> Effect Unit

--boldWrap x = boldWrap2 show x
{-
foreign import hasEvenImpl :: forall f. (f Int -> Int) -> f Int -> Boolean

hasEvenWrap :: forall f. Foldable f => f Int -> Boolean
hasEvenWrap xs = hasEvenImpl product xs

foreign import hasEvenConstraint :: forall f. Foldable f => f Int -> Boolean
-}