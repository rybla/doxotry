module Doxotry.Example.ContDemo where

import Prelude

import Control.Monad.Cont (class MonadCont, Cont, callCC, cont, runCont)
import Data.Foldable (foldMap)
import Effect (Effect)
import Effect.Console (log)

-- | A minimal example demonstrating the control flow of `callCC`.
-- | The `exit` function provided by `callCC` captures the current continuation,
-- | allowing us to escape the computation early (similar to `return` in imperative languages).
example1 :: forall m. MonadCont m => m String
example1 = callCC \exit -> do
  let val = 10

  -- If this condition is met, we call `exit`.
  -- This immediately terminates the `callCC` block, discarding the rest of the do-block.
  -- The value "Early exit" becomes the result of the entire `callCC` expression.
  when (val > 5) do
    exit "Early exit: Value was greater than 5"

  -- This code is unreachable because `exit` was called above.
  pure "Standard exit"

select :: forall r a. Monoid r => Array a -> Cont r a
select xs = cont \k -> foldMap k xs

example2 :: Cont (Array (Array Int)) (Array Int)
example2 = do
  x <- select [ 1, 2, 3 ]
  y <- select [ 10, 20 ]
  pure [ x, y ]

main :: Effect Unit
main = do
  -- We run the generic Cont computation. 
  -- The second argument `identity` is the final continuation (what to do with the result).
  log $ "example1: " <> runCont example1 identity
  log $ "example2: " <> show (runCont example2 (\xs -> [ xs, xs <#> (_ + 1) ]))
