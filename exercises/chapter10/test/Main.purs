module Test.Main where

import Prelude
import Test.Calculate
import Test.Display

import Control.Apply (lift2)
import Control.Promise (toAff, toAffE)
import Data.Argonaut (class DecodeJson, jsonParser)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.List (List(..), (:))
import Data.Map (Map, fromFoldable)
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
type MyMap
  = Map.Map (Set.Set String) Int

myMap :: MyMap
myMap =
  Map.fromFoldable
    [ Tuple (Set.fromFoldable [ "x", "y" ]) 12
    , Tuple (Set.fromFoldable [ "y", "z" ]) 34
    ]

m1 :: Map.Map String Int
m1 = Map.fromFoldable [ "a" /\ 1, "b" /\ 2, "c" /\ 3 ]

m2 = Map.insert "x" 5 m1

m3 = Map.delete "b" m2

type MyRec
  = { a :: Int, b :: Number }

myrec = { a: 1, b: 2.0 }

myrecStr = stringify $ encodeJson myrec

myrecDec :: Effect Unit
myrecDec =
  case jsonParser "{\"b\":2,\"c\":1}" of
    Left e1 -> log $ "Parsing error " <> e1
    Right j -> case decodeJson j of
      Left e2 -> log $ "Decoding error " <> e2
      Right (r :: MyRec) -> log $ "Got record " <> show r

qr :: QuadRec
qr = { a: 1.0, b: 2.0, c: 3.0 }

arr :: Array Int
arr = [ 1, 2, 3 ]

p1 :: Pair Int
p1 = Pair 1 2

-- Kinda annoying needing to do newtype wrapping.
newtype MyPair a
  = MyPair (Pair a)

-- Got sidetracked investigating orphan instances.
instance encodeJsonMyPair :: EncodeJson a => EncodeJson (MyPair a) where
  encodeJson (MyPair (Pair x y)) = encodeJson [ x, y ]

data MixPair a b = MixPair a b

instance showMixPair :: (Show a, Show b) => Show (MixPair a b) where
  show (MixPair a b) = "(MixPair " <> show a <> " " <> show b <> ")"

instance decodeJsonMixPair :: (DecodeJson a, DecodeJson b) => DecodeJson (MixPair a b) where
  decodeJson j = do
    y <- decodeJson j
    case y of
      [a, b] -> lift2 MixPair (decodeJson a) (decodeJson b)
      _ -> Left "Couldn't decode MixPair"
{-
  decodeJson j = do
    y <- decodeJson j
    case y of
      (a : b : Nil) -> MixPair <$> decodeJson a <*> decodeJson b
      _ -> Left "Couldn't decode MixPair"
-}
{-
  decodeJson j = decodeJson j >>= f
    where
    f (a : b : Nil) = MixPair <$> decodeJson a <*> decodeJson b
    f _ = Left "Couldn't decode MixPair"
-}

instance encodeJsonMixPair :: (EncodeJson a, EncodeJson b) => EncodeJson (MixPair a b) where
  encodeJson (MixPair a b) = encodeJson [encodeJson a, encodeJson b]

main :: Effect Unit
main = do
  let
    t = Tuple 1 "Hat"
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
  log $ stringify $ encodeJson { some: 1, record: 2 }
  logShow myrecStr
  myrecDec
  logShow $ showQuadRec qr
  logShow $ sh [ 1, 2, 3 ]
  logShow $ sh t
  logShow $ sh m1
  logShow $ sh m2
  logShow $ sh m3
  --log $ stringify $ encodeJson p1
  log $ stringify $ encodeJson (Tuple 1 2)
  log $ stringify $ encodeJson [ 1, 2 ]
  log $ stringify $ encodeJson (MyPair $ Pair 1 2)
  logShow $ quadraticRoots 1.0 2.0 3.0
  logShow $ myArr []
  logShow $ sum $ myArr []
  logShow $ cumulativeSums [ 1, 2, 3 ]
  logShow $ addComplex { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
  logShow $ cumulativeSumsBroken [ 1, 2, 3 ]
  --logShow $ addComplexBroken { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
  --logShow $ cumulativeSumsDecoded [ 1, 2, 3 ]
  --logShow $ addComplexDecoded { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
  let
    mp :: Map String Int
    mp = fromFoldable [ Tuple "hat" 1, Tuple "cat" 2 ]
  logShow $ mapSetFoo mp
  logShow $ valuesOfMap mp
  logShow $ Set.fromFoldable [1, 3]
  let
    mix = MixPair 1 "Hat"
    menc = encodeJson mix
  logShow mix
  log $ stringify menc
  logShow $ (decodeJson menc :: Either String (MixPair Int String))


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
