module Effect.Storage where

import Prelude

import Effect (Effect)
import Foreign (Foreign)

foreign import setItem :: String -> String -> Effect Unit

foreign import getItem :: String -> Effect Foreign

foreign import removeItem :: String -> Effect Foreign
