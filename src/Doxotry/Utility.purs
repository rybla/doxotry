module Doxotry.Utility where

import Prelude

import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafeCrashWith)

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "[todo] " <> msg

runIdentity :: forall a. Identity a -> a
runIdentity = unwrap

