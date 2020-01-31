module Test.Main where

import Prelude

import Data.Array (foldl, head, nub, tail, (..))
import Data.Foldable (foldM)
import Data.List (List(..), fromFoldable, reverse, (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."

--foldM

cfoldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
cfoldM _ a Nil = pure a
--cfoldM f a (b : bs) = f a b >>= \a' -> foldM f a' bs

cfoldM f a (b : bs) = do
  -- Interesting. The type of a' is without container m
  a' <- f a b
  cfoldM f a' bs

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

--a = foldM safeDivide 100 (fromFoldable [5, 2, 2])

third :: forall a. Array a -> Maybe a
third arr = do
  t1 <- tail arr
  t2 <- tail t1
  head t2

-- Take a subset of input array
moreSums :: Array Int -> Int -> Array Int
moreSums resultsSoFar nextCoin =
  resultsSoFar <> map (add nextCoin) resultsSoFar

sums :: Array Int -> Array Int
sums arr =
  --nub $ foldl moreSums [0] arr
  foldl moreSums [0] arr # nub
  -- Less-readable one-liner
  --foldl (\a b -> a <> map (add b) a) [0] arr # nub
  --nub <<< foldM (\x y -> [x, x+ y]) 0

s2helper :: Int -> Int -> Array Int
s2helper x y =
  [x, x + y]

sums2 :: Array Int -> Array Int
--sums2 arr = foldM (\x y -> [x, x + y]) 0 arr # nub
--sums2 arr = foldM s2helper 0 arr # nub
sums2 arr = foldM s2helper 0 arr # nub

{-
  Interesting how capable bind and do block are.
  Did not expect it to magically be able to unwrap and rewrap array of array so well.
  Bind with array input has an expansion effect.
  Easier to visualize expansion in do block as nested loops.
  Should think of bind as do block too.
-}
e1 :: Array Int
e1 =
  [1,2,3] >>= \x -> [x, x + 10]

e2 :: Array Int
e2 = do
  x <- [1,2,3]
  y <- [x, x + 10]
  --y <- (\x -> [x, x + 10])
  pure y

l :: List Int
l = fromFoldable (1 .. 5)

oddOnly :: Int -> Maybe Boolean
oddOnly a =
  if mod a 2 == 0 then
    Just false
  else
    Just true

{-
foldM   :: (b -> a -> m b) -> b -> f a -> m b

m is Maybe
b is List Int
f a is List Int
a is Int

bind    :: m a -> (a -> m b) -> m b
filter  :: (a ->   Boolean) -> List a ->    List a
filterM :: (a -> m Boolean) -> List a -> m (List a)
-}

--foldM :: forall m a b. Monad m => (b -> a -> m b) -> b -> List a -> m b

--fhelp :: forall m a b. Monad m => b -> a -> m b
--fhelp :: forall m a b. Monad m => b -> a -> m b

{-
  given an (a -> m Boolean), use with foldM which requires
  b -> a -> m b
-}

-- build reversed list, then reverse at end

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM filterfunc xs =
  foldM foldfunc Nil xs <#> reverse
  where
    foldfunc :: List a -> a -> m (List a)
    foldfunc acc x =
      -- Interesting mapping into monad
      map simpleApplyKeep mkeep
      where
        mkeep :: m Boolean
        mkeep = filterfunc x

        simpleApplyKeep :: Boolean -> List a
        simpleApplyKeep true = x : acc
        simpleApplyKeep false = acc

filterM2 :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM2 _ Nil = pure Nil
filterM2 f (x : xs) =
  f x >>= \z ->
    filterM2 f xs >>= \xs' ->
      if z then
        pure (x : xs')
      else
        pure xs'

{- Very interesting that m bool can be tested in do block.
  Takeway lesson:
    When in do block, pretend all m's don't exist.
    Magically works with smattering of m's
-}

filterM3 :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM3 _ Nil = pure Nil
filterM3 f (x : xs) = do
  mkeep <- f x
  others <- filterM3 f xs
  if mkeep then
    pure (x : others)
  else
    pure others

filterM4 :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM4 _ Nil = pure Nil
filterM4 f (x : xs) = do
  mkeep <- f x
  others <- filterM4 f xs
  pure if mkeep then
    (x : others)
  else
    others
