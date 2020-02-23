module Test.Main where

import Prelude

import Control.Monad.Except (except, runExcept)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.List.NonEmpty (head)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Alert (hi, alert)
import Effect.Class.Console (log)
import Foreign.Generic (class Decode, class Encode, F, ForeignError(..), decode, decodeJSON, defaultOptions, encode, encodeJSON, genericDecode, genericEncode)

foreign import foreignLog :: String -> Effect Unit

uncurriedAdd :: Fn2 Int Int Int
uncurriedAdd = mkFn2 \n m -> m + n

curriedAdd :: Int -> Int -> Int
curriedAdd n m = m + n

data Tree a = Leaf a | Branch (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance decodeTree :: Decode a => Decode (Tree a) where
  decode a = genericDecode (defaultOptions { unwrapSingleConstructors = true }) a

instance encodeTree :: Encode a => Encode (Tree a) where
  encode a = genericEncode (defaultOptions { unwrapSingleConstructors = true }) a

instance showTree :: Show a => Show (Tree a) where
  show a = genericShow a

t :: Tree Int
--t = Leaf 1
t = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

-- encode decode
encDecTree :: Tree Int -> Tree Int
encDecTree t1 =
  let
    s = encodeJSON t1
    d = runExcept (decodeJSON s :: F (Tree Int))
  in
    case d of
      Right t2 -> t2
      _ -> Leaf 0

data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

instance showIos :: Show IntOrString where
  show (IntOrString_Int i) = show i
  show (IntOrString_String s) = show s

instance encodeIntOrString :: Encode IntOrString where
  encode (IntOrString_Int i) = encode i
  encode (IntOrString_String s) = encode s

instance decodeIntOrString :: Decode IntOrString where
  decode v =
    let
      dvi = runExcept ((decode v) :: F Int)
      dvs = runExcept ((decode v) :: F String)
    in
      case (Tuple dvi dvs) of
        -- biased towards ints
        Tuple (Right i) _ -> except $ Right $ IntOrString_Int i
        Tuple _ (Right s) -> except $ Right $ IntOrString_String s
        --Tuple (Right i) (Left s) -> except $ Left $ NonEmptyList ((ForeignError $ "decoded both " <> show i <> " " show s) :| Nil)
        Tuple _ _ -> except $ Left $ NonEmptyList ((ForeignError "Could not decode") :| Nil)

encDecIos :: IntOrString -> IntOrString
encDecIos ios =
  let
    s = encodeJSON ios
    d = runExcept (decodeJSON s :: F IntOrString)
  in
    case d of
      Right ios2 -> ios2
      Left er -> IntOrString_String $ show $ head er

ios1 :: IntOrString
ios1 = IntOrString_Int 1
iosfoo :: IntOrString
iosfoo = IntOrString_String "foo"

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ runFn2 uncurriedAdd 3 10
  log $ show $ curriedAdd 3 10
  foreignLog "hello"
  --alershow $ t "something" -- error in console, since not a browser
  log $ show t
  log $ encodeJSON t
  log $ show $ encDecTree t
  log $ show $ ios1
  log $ show $ iosfoo
  log $ encodeJSON $ ios1
  log $ encodeJSON $ iosfoo
  log $ show $ encDecIos $ ios1
  log $ show $ encDecIos $ iosfoo
  log $ show $ runExcept (decodeJSON "[[1, 2, 3], [4, 5], [6]]" :: F (Array (Maybe (Array (Maybe Int)))))
  log $ show $ runExcept (decodeJSON "1" :: F IntOrString)
  --log $ show $ runExcept (decodeJSON "foo" :: F IntOrString)
  log $ encodeJSON "hi"
  log $ encodeJSON $ IntOrString_String "hi"
  log $ show $ runExcept (decodeJSON (encodeJSON $ IntOrString_String "sup") :: F String)
  --log $ show $ runExcept (decodeJSON (encodeJSON "hi") :: F String)
  --log $ show $ runExcept (decodeJSON "foo" :: F String)



