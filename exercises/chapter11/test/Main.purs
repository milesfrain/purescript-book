module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), lift, runExceptT, throwError)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, StateT(..), execState, get, modify, modify_, put, runState, runStateT)
import Control.Monad.Writer (Writer, WriterT(..), execWriter, runWriter, runWriterT, tell)
import Data.Array (many)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, traverse_)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), drop, stripPrefix, take)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Console (log)

sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

sumArrayGeneric :: forall t a. Traversable t => Semiring a => t a -> State a (t a)
sumArrayGeneric = traverse \n -> modify \sum -> sum + n

testParens :: String -> Boolean
testParens str =
  let
    ca = toCharArray str

    openTally :: Char -> Int -> Int
    -- Open parens only considered if not already in deficit
    -- No recovery from too-many closed parens
    openTally '(' tally | tally >= 0 = tally + 1
    openTally ')' tally = tally - 1
    -- Non-parens has no effect
    openTally _ tally = tally

    sumParens :: Array Char -> State Int Unit
    -- Either modify or modify_ are fine
    --sumParens = traverse_ \c -> modify $ openTally c
    sumParens = traverse_ \c -> modify_ $ openTally c

    finalTally :: Int
    finalTally = execState (sumParens ca) 0
  in
    finalTally == 0

line :: String -> Doc
line str = do
  idt <- ask
  --pure $ fold (replicate idt "  " :: Array String) <> str
  pure $ power "  " idt <> str

type Level = Int

type Doc = Reader Level String

indent :: Doc -> Doc
indent = local \l -> l + 1

cat :: Array Doc -> Doc
cat = sequence >>> map (foldMap (_ <> "\n"))
--cat = sequence >>> map (foldMap \s -> s <> "\n")
--cat arr = sequence arr # map (foldMap \s -> s <> "\n")

catVerbose :: Array Doc -> Doc
catVerbose arrayOfDoc =
  let
    readerOfArrayOfString :: Reader Level (Array String)
    readerOfArrayOfString = sequence arrayOfDoc

    flattenStrings :: Array String -> String
    flattenStrings = foldMap (\s -> s <> "\n")

    readerOfBigString :: Doc
    readerOfBigString = map flattenStrings readerOfArrayOfString
  in
    readerOfBigString

render :: Doc -> String
render reader = runReader reader 0

docs :: Doc
docs =
  cat
    [ line "Here is some indented text:"
    , indent $ cat
      [ line "I am indented"
      , line "So am I"
      , indent $ line "I am even more indented"
      , line "Normal indent"
      ]
    , line "No indent"
    ]

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \n -> tell $ Additive n

collatz :: Int -> Int -> Writer (Array Int) Int
collatz 1 i = do
  tell [1]
  pure i
collatz n i = do
  tell [n]
  if mod n 2 == 0
    then collatz (n / 2) (i + 1)
    else collatz (3 * n + 1) (i + 1)

--innerSafeDivide :: Int -> Int -> (ExceptT Int Identity)
--innerSafeDivide n d = do

innerSafeDivide :: Int -> Int -> ExceptT String Identity Int
innerSafeDivide _ 0 = throwError "div 0"
innerSafeDivide n d = pure $ div n d

safeDivide :: Int -> Int -> Either String Int
safeDivide n d = unwrap $ runExceptT (innerSafeDivide n d)

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

splitVerbose :: Parser String
splitVerbose = do
  s <- get
  lift $ tell ["The state is " <> s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)


myRun :: Either (Array String) (Tuple (Tuple String String) (Array String))
myRun = runParser split "test"

-- type variable `a` can either be `String`, `Array String`, `Array (Array String)`, etc.
runParser :: forall a. Parser a -> String -> Either (Array String) (Tuple (Tuple a String) (Array String))
runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

myRunMany :: Either (Array String) (Tuple (Tuple (Array String) String) (Array String))
myRunMany = runParser (many split) "test"

stringVerbose :: String -> Parser String
stringVerbose prefix = do
  full <- get
  lift $ tell ["The state is " <> full]
  case stripPrefix (Pattern prefix) full of
    Nothing -> lift $ lift $ throwError ["Prefix not found"]
    Just suffix -> do
      put suffix
      pure prefix

string :: String -> Parser String
string prefix = do
  full <- get
  tell ["The state is " <> full]
  case stripPrefix (Pattern prefix) full of
    Nothing -> throwError ["Prefix not found"]
    Just suffix -> do
      put suffix
      pure prefix

-- Skipping ReaderT WriterT document rewrite exercise

split :: Parser String
split = do
  s <- get
  tell ["The state is " <> s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

leadingA :: Parser (Array String)
leadingA = many (string "a")

leadingB :: Parser (Array String)
leadingB = many (string "b")

leadingAB :: Parser (Array (Array String))
leadingAB = many (leadingA <|> leadingB)

main :: Effect Unit
main = do
  log "hi"
  --log $ show $ runParser (many (many (string "a") <|> many (string "b"))) "aaabbc"
  log $ show $ runParser leadingAB "aaabbc"
  {-
  log $ show $ safeDivide 13 5
  log $ show $ safeDivide 13 0
  log $ show $ runParser (string "abc") "abcdef"
  log $ show $ runParser (string "nope") "abcdef"
  log $ show $ runParser split "test"
  log $ show $ runParser (many split) "test"
  log $ show $ runState (sumArray [1,2,3]) 0
  log $ show $ runState (sumArrayGeneric [1,2,3]) 0
  log $ show $ testParens ""
  log $ show $ render docs
  log $ show $ execWriter $ sumArrayWriter [1,2,3]
  log $ show $ runWriter $ sumArrayWriter [1,2,3]
  log $ show $ runWriter $ collatz 10 0
  log $ show $ testParens "(()(())())"
  log $ show $ testParens "(()foo((bar))())"
  log $ show $ testParens ")"
  log $ show $ testParens ")("
  log $ show $ testParens "(()()"
  -}

