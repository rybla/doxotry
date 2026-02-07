module Doxotry.Language.Typing where

import Prelude

import Doxotry.Language.Grammar
import Control.Monad.Reader (class MonadReader)
import Doxotry.Utility (todo)

--------------------------------------------------------------------------------

type Ctx = { tyCtx :: TyCtx }

type Env = {}

--------------------------------------------------------------------------------

-- TODO: collect subtyping constraints

check :: forall m ty. MonadReader Ctx m => Ty -> Tm ty -> m (Tm Ty)
check ty (LitTm tm) = do
  tm' <- checkLit ty tm.lit
  pure $ LitTm tm'
check ty (VarTm tm) = todo "" -- TODO: assert subtyping constraint of ascribed type of var to expected type
check ty (AppTm tm) = todo ""
check ty (FunTm tm) = todo ""
check ty (CloTm tm) = todo ""

--------------------------------------------------------------------------------

checkLit :: forall m ty. Monad m => Ty -> Lit -> m (LitTm ty)
checkLit _ _ = todo "checkLit"

