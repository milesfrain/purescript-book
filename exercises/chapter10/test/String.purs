module Test.String where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Generic (class Decode, class Encode, F, decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)

{-
encoded :: String
encoded = encodeJSON t

decoded :: F (Tree Int)
decoded = decodeJSON encoded
-}

v = "hi"
encoded = encodeJSON "hi"

main :: Effect Unit
main = do
  log $ encoded
  log $ show $ runExcept (decodeJSON "foo" :: F String)



