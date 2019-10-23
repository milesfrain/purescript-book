module Test.Main where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (examplePerson, Person(..), person)
import Data.AddressBook.Validation (nonEmpty, validatePerson)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse)
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = logShow (validatePerson examplePerson)

--add :: forall a f. Apply f => f a -> f a -> f a
--add x y = lift2 add x y
-- Hmm, I guess we need to be specific and restrict to Maybe only

-- There is likely a much more generic way to accomplish this
--add :: forall a. Maybe a -> Maybe a -> Maybe a
--addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 add
--add x y = lift2 add x y

addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 add

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 mul

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 sub

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 div

--newtype ApplySemiring = ApplySemiring a
--data ApplySemiring f a = (f a)
--data ApplySemiring = ApplySemiring String
--data ApplySemiring f a = ApplySemiring (Apply f) (Semiring a)
newtype LiftedSemiring f a = LiftedSemiring (f a)

--class (Apply f, Semiring a) <= ApplySemiring
--instance applySemiring :: Apply Semiring where
--instance semiringMaybe :: forall a. Semiring a => Semiring (Maybe a) where
--instance applySemiring :: Apply Int where
--instance applySemiring :: forall a. Semiring a => Apply a where
--instance semiringApply :: forall a. Semiring a => Semiring (Apply a) where
--instance semiringApply :: (Apply f, Semiring a) => Semiring (ApplySemiring f a) where

--instance semiringLifted :: (Apply f, Semiring a) => Semiring (LiftedSemiring f a) where
--instance semiringLifted :: (Applicative f, Semiring a) => Semiring (LiftedSemiring f a) where
{-
    -- this works
instance semiringLifted :: (Applicative f, Semiring a) => Semiring (LiftedSemiring f a) where
  add (LiftedSemiring x) (LiftedSemiring y) = LiftedSemiring (lift2 add x y)
  mul (LiftedSemiring x) (LiftedSemiring y) = LiftedSemiring (lift2 mul x y)
  zero = LiftedSemiring (pure zero)
  one = LiftedSemiring (pure one)
-}

instance semiringLifted :: (Applicative f, Semiring a) => Semiring (LiftedSemiring f a) where
  add (LiftedSemiring x) (LiftedSemiring y) = LiftedSemiring (lift2 add x y)
  mul (LiftedSemiring x) (LiftedSemiring y) = LiftedSemiring (lift2 mul x y)
  zero = LiftedSemiring (pure zero)
  one = LiftedSemiring (pure one)

{-
instance semiringApply :: (Apply f, Semiring a) => Semiring (LiftedSemiring f a) where
  add = lift2 add
  mul = lift2 mul
  zero = zero
  one = one
-}

{-
  Would extending Euclidean Ring work?
  Probably only if there's another special type for (Apply EucideanRing)
-}
--instance appEuclid :: forall a f. EuclideanRing a => Apply f => f a -> f a -> f a
--add x y = lift2 add x y

--addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
--addApply = lift2 add

--instance semiringApply :: Semiring (Apply a)


{-
myadd :: forall a f. Semiring a => Apply f => f a -> f a -> f a
myadd x y = lift2 add x y
-}

data MyType wrapped = First wrapped | Second
instance showMyType :: Show wrapped => Show (MyType wrapped) where
  show (First x) = "(First " <> show x <> ")"
  show Second = "Second"

ts1 :: String
ts1 = show (First 33)

ts2 :: String
ts2 = show (Second :: MyType Int) -- Type annotation rarely necessary

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = map Just x
combineMaybe Nothing = pure Nothing -- Compiler will automatically match f

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance functorTree :: Functor Tree where
  map f Leaf = Leaf
  map f (Branch t1 v t2) = Branch (map f t1) (f v) (map f t2)

exampleTree :: Tree Int
exampleTree = Branch (Branch Leaf 1 Leaf) 3 (Branch Leaf 2 Leaf)

instance showTree :: Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch t1 v t2) = "(Branch " <> show t1 <> " " <> show v <> " " <> show t2 <> ")"

--foldl = (b -> a -> b) -> b -> f a -> b
--foldr = (a -> b -> b) -> b -> f a -> b
-- foldMap has dependency on output being monoid
--foldMap = (a -> m) -> f a -> m

instance foldableTree :: Foldable Tree where
  foldl f acc Leaf = acc
  foldl f acc (Branch t1 v t2) = foldl f (f (foldl f acc t1) v) t2
  -- Not sure how to rearrange this nicely without parens
  --foldl f acc (Branch t1 v t2) = foldl f acc t1 $ flip f v
  foldr f acc Leaf = acc
  foldr f acc (Branch t1 v t2) = foldr f (f v (foldr f acc t2)) t1
  foldMap f Leaf = mempty
  foldMap f (Branch t1 v t2) = foldMap f t1 <> f v <> foldMap f t2

{-
traverse
  (a -> m b) -> t a -> m (t b)
sequence
  t (m a) -> m (t a)

how to combine side effects? Probably applicative do?
Or map <$> and apply <*>

-}
--Traversable t => Applicative m => (a -> m t) -> t a -> t m
--instance semiringLifted :: (Applicative f, Semiring a) => Semiring (LiftedSemiring f a) where
instance traversableTree :: Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Branch t1 v t2) = ado
    mt1 <- traverse f t1
    mv <- f v
    mt2 <- traverse f t2
    in Branch mt1 mv mt2
  -- alternative
  --traverse f (Branch t1 v t2) = Branch <$> traverse f t1 <*> f v <*> traverse f t2

  sequence Leaf = pure Leaf
  sequence (Branch t1 v t2) = ado
    mt1 <- sequence t1
    mv <- v
    mt2 <- sequence t2
    in Branch mt1 mv mt2

traverseWithSequence :: forall m t a b. Applicative m => Traversable t => (a -> m b) -> t a -> m (t b)
traverseWithSequence f t = sequence (f <$> t)

sequenceWithTraverse :: forall m t a. Applicative m => Traversable t => t (m a) -> m (t a)
sequenceWithTraverse t = traverse identity t
