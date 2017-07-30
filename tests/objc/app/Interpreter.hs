{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Management of GHC interpreter sessions through the 'hint' package.
--
-- Interpreter sessions run in their own thread. They receive interpreter commands as monadic 'Interpreter' computations
-- via an inlet 'MVar'. These commands return the result of command execution via another 'MVar' specifically used only
-- for this one command.
--

module Interpreter (
  Session, Result(..),
  start, stop, eval, typeOf, load
) where

  -- standard libraries
import Prelude                      hiding (catch)
import Control.Applicative
import Control.Concurrent
import Control.Exception            (SomeException, evaluate, catch)
import Control.Monad

import System.IO

  -- hint
import qualified Language.Haskell.Interpreter as Interp


-- |Abstract handle of an interpreter session.
--
newtype Session = Session (MVar (Maybe (Interp.Interpreter ())))

-- |Possible results of executing an interpreter action.
--
data Result = Result String
            | Error  String

-- |Start a new interpreter session.
--
start :: IO Session
start
  = do
    { inlet <- newEmptyMVar
    ; forkIO $ void $ Interp.runInterpreter (startSession inlet)
    ; return $ Session inlet
    }
  where
    startSession inlet = Interp.setImports ["Prelude"] >> session inlet
        
    session inlet
      = do
        { maybeCommand <- Interp.lift $ takeMVar inlet
        ; case maybeCommand of
            Nothing      -> return ()
            Just command -> 
              do
              { command
              ; session inlet
              }
        }

-- Terminate an interpreter session.
--
stop :: Session -> IO ()
stop (Session inlet) = putMVar inlet Nothing

-- Evaluate a Haskell expression in the given interpreter session, 'show'ing its result.
--
-- If GHC raises an error, we pretty print it.
--
eval :: Session -> String -> IO Result
eval (Session inlet) e
  = do
    { resultMV <- newEmptyMVar
    ; putMVar inlet $ Just $       -- the interpreter command we send over to the interpreter thread
        do
          {                  -- demand the result to force any contained exceptions
          ; result <- do { !result <- Interp.eval e
                         ; return result }
                      `catch` (return . pprError)
                      `catch` (return . (show :: SomeException -> String))
          ; Interp.lift $ putMVar resultMV (Result result)
          }
    ; takeMVar resultMV
    }

-- Infer the type of a Haskell expression in the given interpreter session.
--
-- If GHC raises an error, we pretty print it.
--
typeOf :: Session -> String -> IO Result
typeOf (Session inlet) e
  = do
    { resultMV <- newEmptyMVar
    ; putMVar inlet $ Just $       -- the interpreter command we send over to the interpreter thread
        do
          {                  -- demand the result to force any contained exceptions
          ; result <- do { !result <- Interp.typeOf e
                         ; return result }
                      `catch` (return . pprError)
                      `catch` (return . (show :: SomeException -> String))
          ; Interp.lift $ putMVar resultMV (Result result)
          }
    ; takeMVar resultMV
    }

-- Load a module into in the given interpreter session.
--
-- If GHC raises an error, we pretty print it.
--
load :: Session -> String -> IO Result
load (Session inlet) mname
  = do
    { resultMV <- newEmptyMVar
    ; putMVar inlet $ Just $       -- the interpreter command we send over to the interpreter thread
        do
          {                  -- demand the result to force any contained exceptions
          ; result <- do { Interp.loadModules [mname]
                         ; mods <- Interp.getLoadedModules
                         ; Interp.setTopLevelModules mods
                         ; return ("Successfully loaded '" ++ mname ++ "'") }
                      `catch` (return . pprError)
                      `catch` (return . (show :: SomeException -> String))
          ; Interp.lift $ putMVar resultMV (Result result)
          }
    ; takeMVar resultMV
    }

pprError :: Interp.InterpreterError -> String
pprError (Interp.UnknownError msg) = msg
pprError (Interp.WontCompile errs) = "Compile time error: \n" ++ unlines (map Interp.errMsg errs)
pprError (Interp.NotAllowed msg)   = "Permission denied: " ++ msg
pprError (Interp.GhcException msg) = "Internal error: " ++ msg
