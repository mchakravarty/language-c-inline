{-# LANGUAGE BangPatterns #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Management of GHC interpreter sessions through the 'hint' package.
--
-- Interpreter sessions run in their own thread. They receive interpreter commands as monadic 'Interpreter' computations
-- via an inlet 'MVar'. These commands return the result of command execution via another 'MVar' specifically used only
-- for this one command.
--

module Interpreter (
  Session,
  new, eval
) where

  -- standard libraries
import Prelude hiding (catch)
import Control.Applicative
import Control.Exception

  -- hint
import qualified Language.Haskell.Interpreter as Interp


-- |Abstract handle of an interpreter session.
--
newtype Session = Session (MVar (Interpreter ()))

-- |Possible results of executing an interpreter action.
--
data Result = Result String
            | Error  String

-- |Create a new interpreter session.
--
new :: IO Session
new
  = do
    { inlet <- newMVar
    ; forkIO $ void $ Interp.runInterpreter (startSession inlet)
    ; return $ Session inlet
    }
  where
    startSession inlet = Interp.setImports ["Prelude"] >> session inlet
        
    session inlet
      = do
        { command <- lift $ takeMVar inlet
        ; command
        ; session inlet
        }

-- Evaluate a Haskell expression in the given interpreter session, 'show'ing its result.
--
-- If GHC raises an error, we pretty print it.
--
eval :: Session -> String -> IO Result
eval (Session inlet) e
  = do
    { resultMV <- newMVar
    ; putMVar inlet $ 
        do
        {   -- demand the result to force any contained exceptions
        ; !result <- Interp.eval e
        ; putMVar $ Result result
        }
        `catch` (return . Error . (show :: SomeException -> Result))
    }

{-
    pprError (Interp.UnknownError msg) = msg
    pprError (Interp.WontCompile errs) = "Compile time error: \n" ++ concatMap Interp.errMsg errs
    pprError (Interp.NotAllowed msg)   = "Permission denied: " ++ msg
    pprError (Interp.GhcException msg) = "Internal error: " ++ msg
-}
