module Test.Main where

import Prelude
import Test.Calculate
import Test.Display

import Control.Promise (toAff)
import Data.Foldable (product, sum)
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (class Foldable1, minimum)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console (clear, log, logShow)
import Effect.Random (random)
import Math (sqrt)

{-
boldWrap2 :: forall a. (a -> String) -> a -> String
boldWrap2 sh x = sh x

boldWrap :: forall a. Show a => a -> String
boldWrap x = boldWrap2 show x
-}

-- Todo, should have a short test section that verifies all examples.

main :: Effect Unit
main = do
  let t = Tuple 1 "Hat"
  logShow t
  log $ boldWrap t
  log $ boldConstraint t
  log $ showEquality Nothing $ Just 5
  yell t
  Aff.launchAff_ $ do
    log "hi"
    toAff $ sleep 1000
    log "bye"
