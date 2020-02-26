module Test.Infer where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runStateT)
import Control.Monad.Writer (runWriterT)
import Data.Newtype (unwrap)

runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s
