module Test.Main where

import Prelude

import Data.Array (foldMap, foldl, foldr)
import Data.Foldable (class Foldable, maximum)
import Data.Hashable (hash, hashEqual, class Hashable)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  logShow (hash 123)
  logShow (hash true)
  logShow (hash [1, 2, 3])
  logShow (hash "testing")
  logShow (hash 'a')
  logShow ("foo" `hashEqual` "foo")
  logShow ("foo" `hashEqual` "bar")

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  --show (Complex { real, imaginary }) = show real <> " + " <> show imaginary <> "i"
  show (Complex c) = show c.real <> " + " <> show c.imaginary <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary

examplec1 :: Complex
examplec1 = Complex {real: 1.0, imaginary: 2.0}
examplec2 :: Complex
examplec2 = Complex {real: 1.0, imaginary: 2.0}
examplec3 :: Complex
examplec3 = Complex {real: 1.1, imaginary: 2.0}

testShow :: String
testShow =
  show examplec1

testEq1 :: Boolean
testEq1 =
  eq examplec1 examplec2

testEq2 :: Boolean
testEq2 =
  eq examplec1 examplec3

data NonEmpty a = NonEmpty a (Array a)

-- Still getting comfortable figuring out what type class constraints are required

--instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
--instance eqNonEmpty :: Eq (NonEmpty a) where -- too much stripped away
  eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

--instance semigroupNonEmpty :: (Semigroup (Array a)) => Semigroup (NonEmpty a) where
instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
--instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (a1 <> [e2] <> a2)

--instance showNonEmpty :: (Show a, Show (Array a)) => Show (NonEmpty a) where
instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty e1 a1) = show e1 <> " " <> show a1

--instance functorNonEmpty :: Functor (Array a) => Functor (NonEmpty a) where
instance functorNonEmpty :: Functor NonEmpty where
  map func (NonEmpty e1 a1) = NonEmpty (func e1) (map func a1)

ne1 :: NonEmpty Int
ne1 = NonEmpty 1 [2]
ne2 :: NonEmpty Int
ne2 = NonEmpty 1 [2]
ne3 :: NonEmpty Int
ne3 = NonEmpty 1 [2, 3]

testNeeq1 :: Boolean
testNeeq1 = ne1 == ne2

testNeeq2 :: Boolean
testNeeq2 = ne1 == ne3

testAppend1 :: NonEmpty Int
testAppend1 = ne1 <> ne2 <> ne3

testMap1 :: NonEmpty Int
testMap1 = map (\x -> x * 2) ne1

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite e1) (Finite e2) = e1 == e2
  eq _ _ = false

{-
  neglecting parenthesis in later lines causes this error in the first correct line:
    Argument list lengths differ in declaration compare
-}
instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare (Finite v1) (Finite v2) = compare v1 v2

-- Wondering why Semigroup wants (NonEmpty a), but Foldable wants NonEmpty
{-
  It's because the definition of Foldable `f` allows inner type `a`.
  While in Semigroup `a`, `a` is the complete type.

class Semigroup a where
  append :: a -> a -> a

class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m

  Lots of ways to accomplish this. Unsure whether cons or append is better
-}
instance foldableNonEmpty :: Foldable NonEmpty where
  foldr func st (NonEmpty val arr) = foldr func st ([val] <> arr)
  foldl func st (NonEmpty val arr) = foldl func st ([val] <> arr)
  foldMap func (NonEmpty val arr) = foldMap func ([val] <> arr)

data OneMore f a = OneMore a (f a)

om1 :: OneMore Array Int
om1 = OneMore 1 [2, 3]

om2 :: OneMore Array String
om2 = OneMore "a" ["b", "c"]

{-


  -}

--instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr func st (OneMore val more) = func val lastb where
    lastb = foldr func st more
  foldl func st (OneMore val more) = foldl func firstb more where
    firstb = (func st val)
  -- Is append really what we want here?
  foldMap func (OneMore val more) = (func val) <> (foldMap func more)
  {-
    Why not just append for foldr, foldl? Efficiency?
    Answer:
    Not obvious that cons might not work for input type,
    and can't use append without wrapping `val` in a container,
    and container of `more` might not be a Semigroup (supports append).
    For example Maybe and Tuple are Foldable, but not Semigroup.
    Monoid type of foldMap output ensures that in can be appended.
    Monoid is a subclass of Semigroup.
  -}
  --foldl func st (OneMore val more) = foldl func st (val : more)

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum arr =
  case maximum arr of
    Just m -> m
{-
  call with
  import Partial.Unsafe (unsafePartial)
  unsafePartial (unsafeMaximum [1,2,3])
-}

--instance ordExtended :: Ord a => Ord (Extended a) where
--  compare Infinite Infinite = EQ

{-
instance cmpHashable :: Hashable a => Ord a => Ord a where
--instance cmpHashable :: Ord a => Ord (Hashable a) where
  if
  compare Infinite Infinite = EQ
-}

-- don't want to replace regular compare, since that's already used
-- Compare two things that are already hashable and orderable
{-
compareHashable :: forall a. Hashable a => Eq a => a -> a -> Ordering
compareHashable a b =
  let
    ha = hash a
    hb = hash b
  in
    if ha == hb && a == b then
      EQ
    else
      compare ha hb -- This will erroneously return EQ for collisions
-}

{-
nubBy is faster because it can sort
nubByEq is slower, but seems to fit better with hashEqual
-}

hashEqualBackup :: forall a. Hashable a => Eq a => a -> a -> Boolean
hashEqualBackup a b =
  hash a == hash b && a == b

{-
import Data.Hashable (hashEqual)
nubByEq hashEqual [65536 ,1, 1, 2]
nubByEq hashEqualBackup [65536 ,1, 1, 2]
-}

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour h) = hash $ mod h 12
