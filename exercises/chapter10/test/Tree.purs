module Test.Tree where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Generic (class Decode, class Encode, F, decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)

data Tree a = Leaf a | Branch (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance decodeTree :: Decode a => Decode (Tree a) where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance encodeTree :: Encode a => Encode (Tree a) where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

instance showTree :: Show a => Show (Tree a) where
  show = genericShow

t :: Tree Int
t = Leaf 1

{-
encoded :: String
encoded = encodeJSON t

decoded :: F (Tree Int)
decoded = decodeJSON encoded
-}

main :: Effect Unit
main = do
  log "hi"
  --log $ show t
  --log $ encordeJSON t



