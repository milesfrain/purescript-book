module Test.Display where

import Prelude

import Effect (Effect)

foreign import boldImpl :: forall a. (a -> String) -> a -> String

boldWrap :: forall a. Show a => a -> String
boldWrap x = boldImpl show x

foreign import boldConstraint :: forall a. Show a => a -> String

foreign import showEquality :: forall a. Eq a => Show a => a -> a -> String

foreign import yell :: forall a. Show a => a -> Effect Unit

