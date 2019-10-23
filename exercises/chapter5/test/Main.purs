module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Picture (Point(..), Shape(..), Picture, bounds, showBounds)
import Effect (Effect)
import Effect.Class.Console (log)

circle :: Shape
circle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

rectangle :: Shape
rectangle = Rectangle (Point { x: 10.0, y: 10.0 }) 10.0 10.0

picture :: Picture
picture = [circle, rectangle]

main :: Effect Unit
main = log (showBounds (bounds picture))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n -1)


{-
pascal :: Int -> Array Int
pascal 1 = [1]
pascal n = do
  -- try to add shifted to previous
  v <- pascal n - 1
-}

type Foo = {a :: Int, b :: Int}
b5 :: Foo -> Boolean
b5 {b: 5} = true
b5 _ = false

{- not pattern matching syntax
b6 :: Foo -> Boolean
b6 (f.b == 6) = true
b6 _ = false
-}

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

-- partial v1
--sameCity { address: { city: p2.address.city}}
-- v2
--sameCity { address: { city: c } } { address: { city: c } } = true
--sameCity _ = false
-- v3
sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } }
  | c1 == c2 = true
  | otherwise = false

testSameCity :: Boolean
testSameCity =
  let
    a1 :: Address
    a1 = { street: "1st", city: "Springfield"}
    a2 :: Address
    a2 = { street: "1st", city: "Springfiels"}
    p1 :: Person
    p1 = { name: "Phil", address: a1 }
    p2 = { name: "John", address: a2 }
  in
    sameCity p1 p2

{-
q2
-- sameCity
forall c a1 a2 p1 p2. Eq c => { address :: { city :: c, | a1} | p1 } { address :: { city :: c, | a2} | p2 } -> Boolean

-- livesInLA
forall a1 p1. { address :: { city :: String, | a1} | p1 }
-}

fromSingleton :: forall a. a -> Array a -> a
fromSingleton d [x] = x
fromSingleton d _ = d

circ1 :: Shape
circ1 = Circle (Point { x: 0.0, y: 0.0 }) 10.0

scale2 :: Shape -> Shape
scale2 (Circle o r) = Circle o (r * 2.0)
scale2 (Rectangle o l w) = Rectangle o (l * 2.0) (w * 2.0)
scale2 (Line (Point {x: x1, y: y1}) (Point {x: x2, y: y2})) = Line (Point {x: x1, y: y1}) (Point {x: x2, y: y2})
scale2 (Text p s) = Text p s -- tedeous wrap-up

gettext :: Shape -> Maybe String
gettext (Text p s) = Just s
gettext _ = Nothing