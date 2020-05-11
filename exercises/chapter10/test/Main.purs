module Test.Main where

import Prelude
import Test.Calculate
import Test.Display

import Control.Promise (toAff, toAffE)
import Data.Argonaut (jsonParser)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console (clear, log, logShow)


-- Todo, should have a short test section that verifies all examples.

type MyMap = Map.Map (Set.Set String) Int

myMap :: MyMap
myMap = Map.fromFoldable
  [ Tuple (Set.fromFoldable ["x", "y"]) 12
  , Tuple (Set.fromFoldable ["y", "z"]) 34
  ]

m1 :: Map.Map String Int
m1 = Map.fromFoldable ["a" /\ 1, "b" /\ 2, "c" /\ 3]

m2 = Map.insert "x" 5 m1

m3 = Map.delete "b" m2

type MyRec = {a :: Int, b :: Number}
myrec = {a : 1, b: 2.0}

myrecStr = stringify $ encodeJson myrec

myrecDec :: Effect Unit
myrecDec =
  --case jsonParser myrecStr of
  --case jsonParser "{\"b\":2,\"a\":1}" of
  case jsonParser "{\"b\":2,\"c\":1}" of
    Left e1 -> log $ "Parsing error " <> e1
    Right j -> case decodeJson j of
      Left e2 -> log $ "Decoding error " <> e2
      Right (r :: MyRec) -> log $ "Got record " <> show r

qr :: QuadRec
qr = {a : 1.0, b : 2.0, c : 3.0}

arr :: Array Int
arr = [1, 2, 3]

p1 :: Pair Int
p1 = Pair 1 2

-- Kinda annoying needing to do newtype wrapping.
newtype MyPair a = MyPair (Pair a)

instance encodeJsonMyPair :: EncodeJson a => EncodeJson (MyPair a) where
  encodeJson (MyPair (Pair x y)) = encodeJson [x, y]

main :: Effect Unit
main = do
  let t = Tuple 1 "Hat"
  logShow t
  log $ boldWrap t
  log $ boldConstraint t
  log $ showEquality Nothing $ Just 5
  yell t
  logShow myMap
  logShow m1
  logShow m2
  logShow m3
  --log $ JSON.writeJSON m1 -- Doesn't quite work
  log $ stringify $ encodeJson m3
  log $ stringify $ encodeJson {some: 1, record: 2}
  logShow myrecStr
  myrecDec
  logShow $ showQuadRec qr
  logShow $ sh [1, 2, 3]
  logShow $ sh t
  logShow $ sh m1
  logShow $ sh m2
  logShow $ sh m3
  --log $ stringify $ encodeJson p1
  log $ stringify $ encodeJson (Tuple 1 2)
  log $ stringify $ encodeJson [1, 2]
  log $ stringify $ encodeJson (MyPair $ Pair 1 2)
  {-
  Aff.launchAff_ do
    log "waiting"
    toAff $ sleep 300
    log "done waiting"
    res <- toAff $ diagonalAsync 3.0 4.0
    logShow res
    resE <- toAffE $ diagonalAsyncEffect 3.0 4.0
    logShow resE
  -}
