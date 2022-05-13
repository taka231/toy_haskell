{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Compile where
import           AST                        (Expr (BiOp, Bool, If, Num))
import           Control.Monad.Fix          (MonadFix)
import           Data.Functor.Identity      ()
import           Data.Text.Internal.Lazy    ()
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy.IO          as LT
import           LLVM.AST                   (Operand, Type (IntegerType))
import           LLVM.AST.IntegerPredicate  (IntegerPredicate (EQ, SGT, SLT, UGT, ULT))
import           LLVM.AST.Type              as AST ()
import           LLVM.AST.Typed
import           LLVM.IRBuilder.Constant    (bit, int32)
import           LLVM.IRBuilder.Instruction (add, br, condBr, icmp, mul, phi,
                                             sdiv, sub)
import           LLVM.IRBuilder.Module      ()
import           LLVM.IRBuilder.Monad       (MonadIRBuilder, block,
                                             currentBlock, named)
import           LLVM.Pretty                ()

class Compile ast where
    compile :: (MonadFix m, MonadIRBuilder m) => ast -> m Operand

instance Compile Expr where
    -- 整数 n をコンパイルした結果は
    compile (Num n) = pure $ int32 n    -- int32 n である
    -- e1 + e2 をコンパイルした結果は
    compile (BiOp e1 op e2) = do
        e1' <- compile e1
        e2' <- compile e2
        case op of
            "+"  -> add e1' e2'
            "-"  -> sub e1' e2'
            "*"  -> mul e1' e2'
            "/"  -> sdiv e1' e2'
            "<"  -> case (typeOf e1', typeOf e2') of
                        (IntegerType 32, IntegerType 32) -> icmp SLT e1' e2'
                        (IntegerType 1, IntegerType 1)   -> icmp ULT e1' e2'
            ">"  -> case (typeOf e1', typeOf e2') of
                        (IntegerType 32, IntegerType 32) -> icmp SGT e1' e2'
                        (IntegerType 1, IntegerType 1)   -> icmp UGT e1' e2'
            "==" -> icmp LLVM.AST.IntegerPredicate.EQ e1' e2'
            _    -> undefined
    compile (Bool True) = pure $ bit 1
    compile (Bool False) = pure $ bit 0
    compile (If condExpr thenExpr elseExpr) = mdo
        cond <- compile condExpr     -- %0 = icmp slt i32 1, 2
        condBr cond ifThen ifElse        -- br i1 %0, label %then, label %else
        -- then:
        ifThen <- block `named` "then"   -- then:
        oprThen <- compile thenExpr  -- then 部分の式を計算する
        br ifEnd                         -- br label %end
        enfOfThen <- currentBlock        -- 現在のブロックを enfOfThen とする
        -- else:
        ifElse <- block `named` "else"   -- else:
        oprElse <- compile elseExpr  -- else 部分の式を計算する
        br ifEnd                         -- br label %end
        endOfElse <- currentBlock        -- 現在のブロックを enfOfElse とする
        -- end:
        ifEnd <- block `named` "end"     -- end:
        -- phi
        phi [(oprThen, enfOfThen), (oprElse, endOfElse)] -- phi
