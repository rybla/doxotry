module Doxotry.Language.Common where

import Prelude

import Control.Monad.Writer (class MonadWriter, tell)
import Data.Newtype (class Newtype)

newtype Log = Log { label :: String, message :: String }

derive instance Newtype Log _

derive newtype instance Show Log

prettyLog :: Log -> String
prettyLog (Log log) = "[" <> log.label <> "] " <> log.message

mkLog :: String -> String -> Log
mkLog label message = Log { label, message }

tellLog :: forall m. MonadWriter (Array Log) m => String -> String -> m Unit
tellLog label message = tell [ mkLog label message ]

