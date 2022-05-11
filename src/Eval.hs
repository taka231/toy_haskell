{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Eval where

import           AST
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.Map               as Map
import           Data.Text

data EvalError =
  EvalTypeError
  | VarNotFound
  deriving (Show, Eq)

type Env = Map.Map Text Value
type EnvRef =  [IORef Env]
type Eval' a = ReaderT EnvRef (ExceptT EvalError IO) a

class Eval ast where
  eval :: ast -> Eval' Value
--
-- runEval :: EnvRef -> Expr -> IO (Either EvalError Value)
-- runEval env expr = runExceptT (runReaderT (eval expr) env)
--
-- envLookup :: Text -> Eval' (Maybe Value)
-- envLookup = do
--     envRef <- ask
--     undefined
-- --
-- instance Eval Expr where
--   eval (Num num) = pure $ VNum num
--   eval (Bool bool) = pure $ VBool bool
--   eval (BiOp lh op rh) = do
--     lhval <- eval lh
--     rhval <- eval rh
--     case (lhval, rhval) of
--       (VNum lhnum, VNum rhnum) -> case op of
--         "+"  -> pure $ VNum $ lhnum + rhnum
--         "-"  -> pure $ VNum $ lhnum - rhnum
--         "*"  -> pure $ VNum $ lhnum * rhnum
--         "/"  -> pure $ VNum $ lhnum `div` rhnum
--         "==" -> pure $ VBool $ lhnum == rhnum
--         "<"  -> pure $ VBool $ lhnum < rhnum
--         ">"  -> pure $ VBool $ lhnum > rhnum
--         "<=" -> pure $ VBool $ lhnum <= rhnum
--         ">=" -> pure $ VBool $ lhnum >= rhnum
--         _    -> throwError EvalTypeError
--       (VBool lhbool, VBool rhbool) -> case op of
--         "==" -> pure $ VBool $ lhbool == rhbool
--         "<"  -> pure $ VBool $ lhbool < rhbool
--         ">"  -> pure $ VBool $ lhbool > rhbool
--         "<=" -> pure $ VBool $ lhbool <= rhbool
--         ">=" -> pure $ VBool $ lhbool >= rhbool
--         _    -> throwError EvalTypeError
--       _ -> throwError EvalTypeError
--   eval (If cond expr1 expr2) = do
--     condval <- eval cond
--     exprval1 <- eval expr1
--     exprval2 <- eval expr2
--     case condval of
--       (VBool vbool) ->
--         if vbool then pure exprval1 else pure exprval2
--       _ -> throwError EvalTypeError
--   eval (Var var_name) = do
--     val <- envLookup var_name
--     case val of
--       Nothing    -> throwError VarNotFound
--       Just value -> pure value
--
--   eval Unit = pure VUnit
--
