module Plots where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (Context2D, Dimensions, getCanvasDimensions, getCanvasElementById, getContext2D, lineTo, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

type Point = { x :: Number, y :: Number }

-- Function should have range of 0-1 over domain of 0-1
f :: Number -> Point
f x = { x, y: 0.5 * Math.sin (Math.tau * x) + 0.5 }

-- Expand 0-1 point range to fill entire canvas. Also flip y axis.
scalePointToCanvas :: Dimensions -> Point -> Point
scalePointToCanvas dim p =
  { x: dim.width  * p.x
  , y: dim.height * (1.0 - p.y)
  }

renderPath :: Context2D -> Array Point -> Effect Unit
renderPath ctx arr =
  strokePath ctx $ for_ arr \p -> do
      lineTo ctx p.x p.y

-- Generate n + 1 equally-spaced numbers between 0 and 1
spacedNumbers :: Int -> Array Number
spacedNumbers n =
  map (toNumber >>> flip (/) (toNumber n)) (0 .. n)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  dim <- getCanvasDimensions canvas
  renderPath ctx $ map (f >>> scalePointToCanvas dim) $ spacedNumbers 50