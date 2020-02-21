module Example.Rectangle where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"

  fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  fillPath ctx $ do
    rect ctx { x: 50.0, y: 50.0, width: 10.0, height: 10.0 }
    rect ctx { x: 70.0, y: 70.0, width: 10.0, height: 10.0 }