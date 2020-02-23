module Effect.Alert where

import Prelude

import Effect (Effect)

foreign import alert :: String -> Effect Unit

foreign import confirm :: String -> Effect Unit

hi :: String
hi = "hi"