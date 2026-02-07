module Doxotry.Utility where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "[todo] " <> msg

