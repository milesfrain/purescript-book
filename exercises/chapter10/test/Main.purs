module Test.Main where

import Prelude
import Test.Examples
import Test.Solutions
import Control.Promise (toAff, toAffE)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error, error, message, throw, try)
import Effect.Uncurried (runEffectFn2)
import Test.URI (encodeURIComponent)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    suite "Chapter Examples" do
      test "uri"
        $ Assert.equal "Hello%20World"
        $ encodeURIComponent "Hello World"
      test "square"
        $ Assert.equal 25.0
        $ square 5.0
      test "diagonal"
        $ Assert.equal 5.0
        $ runFn2 diagonal 3.0 4.0
      test "diagonalNested"
        $ Assert.equal 5.0
        $ diagonalNested 3.0 4.0
      test "diagonalArrow"
        $ Assert.equal 5.0
        $ diagonalArrow 3.0 4.0
      test "uncurriedAdd"
        $ Assert.equal 13
        $ runFn2 uncurriedAdd 3 10
      test "curriedAdd"
        $ Assert.equal 13
        $ curriedAdd 3 10
      test "cumulativeSums"
        $ Assert.equal [ 1, 3, 6 ]
        $ cumulativeSums [ 1, 2, 3 ]
      test "addComplex"
        $ Assert.equal { imag: 6.0, real: 4.0 }
        $ addComplex { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
      test "maybeHead - Just"
        $ Assert.equal (Just 1)
        $ maybeHead [ 1, 2, 3 ]
      test "maybeHead - Nothing"
        $ Assert.equal Nothing
        $ maybeHead ([] :: Array Int)
      test "isEmpty - false"
        $ Assert.equal false
        $ isEmpty [ 1, 2, 3 ]
      test "isEmpty - true"
        $ Assert.equal true
        $ isEmpty []
      -- It is not possible to test the thrown exception
      -- by catching with a `try` because `unsafeHead`
      -- lacks `Effect` in its return type.
      -- Lifting with `pure` doesn't help in this situation.
      test "unsafeHead - value"
        $ Assert.equal 1
        $ unsafeHead [ 1, 2, 3 ]
      test "boldWrap"
        $ Assert.equal "(TUPLE 1 \"HAT\")!!!"
        $ boldWrap
        $ Tuple 1 "Hat"
      test "boldConstraint"
        $ Assert.equal "(TUPLE 1 \"HAT\")!!!"
        $ boldConstraint
        $ Tuple 1 "Hat"
      test "showEquality - not equal"
        $ Assert.equal "Nothing is not equal to (Just 5)"
        $ showEquality Nothing (Just 5)
      test "showEquality - equivalent"
        $ Assert.equal "Equivalent"
        $ showEquality [ 1, 2 ] [ 1, 2 ]
      -- Cannot test for actual logged value
      test "yell" do
        result <- liftEffect $ yell $ Tuple 1 "Hat"
        Assert.equal unit result
      test "diagonalLog" do
        result <- liftEffect $ runEffectFn2 diagonalLog 3.0 4.0
        Assert.equal 5.0 result
      test "sleep" do
        result <- toAff $ sleep 1
        Assert.equal unit result
      test "diagonalAsync" do
        result <- toAff $ diagonalAsync 1 3.0 4.0
        Assert.equal 5.0 result
      test "diagonalAsyncEffect" do
        result <- toAffE $ diagonalAsyncEffect 1 3.0 4.0
        Assert.equal 5.0 result
      suite "cumulativeSums Json" do
        test "broken"
          $ Assert.equal (Left "Couldn't decode Array (Failed at index 3): Value is not a Number")
          $ cumulativeSumsDecodedBroken [ 1, 2, 3 ]
        test "working"
          $ Assert.equal (Right [ 1, 3, 6 ])
          $ cumulativeSumsDecodedWorking [ 1, 2, 3 ]
      suite "addComplex Json" do
        test "broken"
          $ Assert.equal (Left "JSON was missing expected field: imag")
          $ addComplexDecodedBroken { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
        test "working"
          $ Assert.equal (Right { imag: 6.0, real: 4.0 })
          $ addComplexDecodedWorking { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 }
    -- Actual exercises
    suite "Exercise Group - Calling JavaScript" do
      suite "Exercise 1 - volumeFn" do
        test "1 2 3"
          $ Assert.equal 6.0
          $ runFn3 volumeFn 1.0 2.0 3.0
        test "1 0 3"
          $ Assert.equal 0.0
          $ runFn3 volumeFn 1.0 0.0 3.0
      suite "Exercise 2 - volumeArrow" do
        test "1 2 3"
          $ Assert.equal 6.0
          $ volumeArrow 1.0 2.0 3.0
        test "1 0 3"
          $ Assert.equal 0.0
          $ volumeArrow 1.0 0.0 3.0
    suite "Exercise Group - Passing Simple Types" do
      suite "Exercise 1 - cumulativeSumsComplex" do
        test "sequential"
          $ Assert.equal
              [ { real: 1.0, imag: 2.0 }
              , { real: 4.0, imag: 6.0 }
              , { real: 9.0, imag: 12.0 }
              ]
          $ cumulativeSumsComplex
              [ { real: 1.0, imag: 2.0 }
              , { real: 3.0, imag: 4.0 }
              , { real: 5.0, imag: 6.0 }
              ]
    suite "Exercise Group - Beyond Simple Types" do
      suite "Exercise 1 - quadraticRoots" do
        test "Real"
          $ Assert.equal
              ( orderCpx
                  $ Pair
                      { real: 1.0, imag: 0.0 }
                      { real: -3.0, imag: 0.0 }
              )
          $ orderCpx
          $ quadraticRoots { a: 1.0, b: 2.0, c: -3.0 }
        test "Imaginary"
          $ Assert.equal
              ( orderCpx
                  $ Pair
                      { real: 0.0, imag: 2.0 }
                      { real: 0.0, imag: -2.0 }
              )
          $ orderCpx
          $ quadraticRoots { a: 4.0, b: 0.0, c: 16.0 }
        test "Complex"
          $ Assert.equal
              ( orderCpx
                  $ Pair
                      { real: -0.5, imag: 1.5 }
                      { real: -0.5, imag: -1.5 }
              )
          $ orderCpx
          $ quadraticRoots { a: 2.0, b: 2.0, c: 5.0 }
        test "Repeated"
          $ Assert.equal
              ( orderCpx
                  $ Pair
                      { real: 1.0, imag: 0.0 }
                      { real: 1.0, imag: 0.0 }
              )
          $ orderCpx
          $ quadraticRoots { a: 3.0, b: -6.0, c: 3.0 }

{-  Move this block comment starting point to enable more tests
-}
-- Put in ascending order by real, then imag components
orderCpx :: Pair Complex -> Pair Complex
orderCpx (Pair c1 c2)
  | c1.real < c2.real = Pair c1 c2
  | c1.real > c2.real = Pair c2 c1
  | c1.imag < c2.imag = Pair c1 c2
  | otherwise = Pair c2 c1
