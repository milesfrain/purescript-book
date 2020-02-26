module Test.Hole where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (many)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Tuple (Tuple)
import Data.Newtype (unwrap)
import Data.String (drop, take)

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split :: Parser String
split = do
  s <- get
  tell ["The state is " <> s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

-- type variable `a` can either be `String` or `Array String`
runParser :: forall a. Parser a -> String -> Either (Array String) (Tuple (Tuple a String) (Array String))
runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

myRunSingle :: Either (Array String) (Tuple (Tuple String String) (Array String))
myRunSingle = runParser split "test"

--ms :: Parser String
ms :: Parser (Array String)
ms = (many split)

myRunMany :: Either (Array String) (Tuple (Tuple (Array String) String) (Array String))
myRunMany = runParser (many split) "test"
