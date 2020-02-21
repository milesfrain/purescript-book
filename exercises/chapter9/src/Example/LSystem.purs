module Example.LSystem where

import Prelude

import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, setShadowBlur, setShadowColor, setShadowOffsetX, setShadowOffsetY, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

buildSentence :: forall a.
  Array a ->
  (a -> Array a) ->
  Int ->
  Array a
buildSentence init prod n =
  go init n
  where
    go s 0 = s
    go s m = go (concatMap prod s) (m - 1)

data Letter = L | R | F | M

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    initial :: Sentence
    --initial = [F, R, R, F, R, R, F, R, R]
    initial = [M]

    productions :: Letter -> Sentence
    productions L = [L]
    productions R = [R]
    --productions F = [F, L, F, R, R, F, L, F]
    productions F = [F, L, M, L, F, R, M, R, F, R, M, R, F, L, M, L, F]
    productions M = [M, R, F, R, M, L, F, L, M, L, F, L, M, R, F, R, M]

    interpret :: State -> Letter -> Effect State
    interpret state L = pure $ state { theta = state.theta - Math.tau / 6.0 }
    interpret state R = pure $ state { theta = state.theta + Math.tau / 6.0 }
    interpret state _ = do -- accounts for both F and M
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      lineTo ctx x y
      pure { x, y, theta: state.theta }

    initialState :: State
    initialState = { x: 120.0, y: 200.0, theta: 0.0 }

  -- Last pattern looks best without fill and shadow
  --setFillStyle ctx "#00F"
  --fillPath ctx do
  strokePath ctx do
    moveTo ctx initialState.x initialState.y
    foldM interpret initialState $ buildSentence initial productions 4
    --closePath ctx
    --setShadowColor ctx "#F00"
    --setShadowOffsetX ctx 5.0
    --setShadowOffsetY ctx 5.0
    --setShadowBlur ctx 3.0
