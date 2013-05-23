-- HSApp: a simple Cocoa app in Haskell
--
-- Management of GHC interpreter session through the 'hint' package.

module Interpreter (
  eval
) where

  -- standard libraries
import Control.Applicative

  -- hint
import qualified Language.Haskell.Interpreter as Interp


-- Evaluate a Haskell expression, 'show'ing its result.
--
-- Each time this function is called a new interpreter context is launched. No state is kept between two subsequent
-- evaluations.
--
-- If GHC raises an error, we pretty print it.
--
eval :: String -> IO String
eval e
  = either pprError id <$> (Interp.runInterpreter $ do
      { Interp.setImports ["Prelude"]
      ; Interp.eval e
      })
  where
    pprError (Interp.UnknownError msg) = msg
    pprError (Interp.WontCompile errs) = "Compile time error: \n" ++ concatMap Interp.errMsg errs
    pprError (Interp.NotAllowed msg)   = "Permission denied: " ++ msg
    pprError (Interp.GhcException msg) = "Internal error: " ++ msg
