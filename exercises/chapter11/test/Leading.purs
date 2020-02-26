module Test.Leading where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (length, many)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripPrefix)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

type Result a = Either (Errors) (Tuple (Tuple a String) (Log))

string :: String -> Parser String
string prefix = do
  full <- get
  tell ["The state is " <> full]
  case stripPrefix (Pattern prefix) full of
    Nothing -> throwError ["Prefix not found"]
    Just suffix -> do
      put suffix
      pure prefix

-- type variable `a` can either be `String`, `Array String`, `Array (Array String)`, etc.
--runParser :: forall a. Parser a -> String -> Either (Errors) (Tuple (Tuple a String) (Log))
runParser :: forall a. Parser a -> String -> Result a
runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

leadingA :: Parser (Array String)
leadingA = many (string "a")

leadingB :: Parser (Array String)
leadingB = many (string "b")

-- Original version that results in runtime recursion error
--leadingAB :: Parser (Array (Array String))
--leadingAB = many (leadingA <|> leadingB)

-- Assuming output should be something like "aaabbc" to Tuple [["a", "a", "a"], ["b", "b"]] "c"
-- Also, whether 2 or more are required, and whether a's must come before b's
-- This is why accompanying unit tests are important

-- Version from Becky Corning
-- https://github.com/beckyconning/purescript-by-example/blob/master/chapter11/src/String.purs
-- Not quite right
leadingAB :: Parser (Array String)
leadingAB = leadingA >>= \_ -> leadingB

isAsThenBs :: String -> Boolean
--isAsThenBs = interpretResult <<< runParser (f "a" >>= \_ -> f "b")
isAsThenBs str = interpretResult $ runParser parser str
--isAsThenBs str = true
  where
  f :: String -> Parser (Array String)
  --f = many <<< string
  f s = many (string s)

  parser :: Parser (Array String)
  parser = (f "a" >>= \_ -> f "b")

  result :: Result (Array String)
  result = runParser parser str

  hasAtLeastTwoElements :: forall a. Array a -> Boolean
  hasAtLeastTwoElements = (_ < 2) <<< length

  --interpretResult :: Either String (Tuple (Tuple (Array String) String) (Array String)) -> Boolean
  interpretResult :: Result (Array String) -> Boolean
  interpretResult (Left _)                                                             = false
  interpretResult (Right (Tuple (Tuple xs y) _)) | hasAtLeastTwoElements xs || y /= "" = false
  interpretResult _                                                                    = true

main :: Effect Unit
main = do
  log $ show $ runParser (string "abc") "abcdef"
  log $ show $ runParser leadingAB "aaabbc"
  log $ show $ isAsThenBs "aaabbc"

-- spago run --main Test.Leading
