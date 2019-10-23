module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concat, concatMap, filter, head, length, range, tail, (..), (:))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Path (Path, filename, isDirectory, ls, root, size)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- Could have simpler `do` notation with `allFiles`, but filter is likely best
onlyFiles :: Path -> Array Path
onlyFiles path =
  let
    onlyFiles' acc file =
      case ls file of
        [] -> file : acc
        children -> do
          child <- children
          onlyFiles' acc child
  in
    onlyFiles' [] path

onlyFilesAlt :: Path -> Array Path
onlyFilesAlt path = filter (not isDirectory) (allFiles root)

largest :: Maybe Path
largest =
  let
    files = onlyFiles root
    fhead = head files
  in
    case fhead of
      Nothing -> Nothing
      Just file -> Just (foldl (\f1 f2 -> if size f1 > size f2 then f1 else f2) file files)

smallest :: Maybe Path
smallest =
  let
    files = onlyFiles root
    fhead = head files
  in
    case fhead of
      Nothing -> Nothing
      Just file -> Just (foldl (\f1 f2 -> if size f1 < size f2 then f1 else f2) file files)

-- This does not work
-- emiel's example is much better
whereIs :: String -> Maybe Path
whereIs target =
  let
    whereIs' parent =
      let
        children = ls parent
        foldfunc :: Maybe Path -> Path -> Maybe Path
        foldfunc saved child =
          if (isNothing saved) && (filename child == target) then
            Just parent else whereIs' child

        maybeMatch = foldl foldfunc Nothing children
      in
        maybeMatch
  in
    whereIs' root




-- only works for positive numbers
isEven :: Int -> Boolean
isEven n = isEvenInner true n
isEvenInner :: Boolean -> Int -> Boolean
isEvenInner evenSubs 0 = evenSubs
isEvenInner evenSubs n = isEvenInner (not evenSubs) (n - 1)

countEven :: Array Int -> Int
countEven a =
  case head a of
    Just h -> (if isEven h then 1 else 0) + countEven (fromMaybe [] $ tail a)
    Nothing -> 0

--squares :: forall n. Array n -> Array n
squares :: Array Int -> Array Int
squares a =
  map (\x -> x * x) a

nonneg :: Array Int -> Array Int
nonneg a =
  filter (\x -> x >= 0) a

myr :: Array Int
myr = range 1 5

infix 1 filter as <$?>

nonneg2 :: Array Int -> Array Int
nonneg2 a =
  (\x -> x >= 0) <$?> a

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

cartp :: Array Int -> Array Int -> Array (Array Int)
cartp a b = do
  i <- a
  j <- b
  pure [i, j]

ptrip :: Int -> Array (Array Int)
ptrip n = do
  i <- (1 .. n)
  j <- (i .. n)
  k <- (1 .. n)
  guard $ i*i + j*j == k*k
  pure [i, j, k]

--factorizations :: Int -> Array (Array Int)
--factorizations :: Int -> Int
factorizations :: Int -> Array Int
factorizations n =
  if n == 1 then
    [1]
  else
    let
      candidates = do
        i <- (2 .. n)
        guard $ n `mod` i == 0
        pure i
      h = head candidates
      firstfactor = fromMaybe n h
      otherfactors = n / firstfactor
    in
      concat [[firstfactor], factorizations otherfactors]

msum :: Array Int -> Int
msum arr =
  foldl (\acc v -> acc + v) 0 arr

-- Better tail recursion example
f :: Int -> Int -> Int
f 0 acc = acc
f n acc = f (n - 1) (acc + 1)

alltrue :: Array Boolean -> Boolean
alltrue arr = foldl (&&) true arr
-- arr is redundant

fibtr :: Int -> Int
fibtr n =
  let
    fibtr' 1 acc1 acc2 = acc1
    fibtr' 2 acc1 acc2 = acc2
    fibtr' nn acc1 acc2 =
      fibtr' (nn - 1) acc2 (acc1 + acc2)
  in
    fibtr' n 1 1

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []
