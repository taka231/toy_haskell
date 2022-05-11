module Compile where
import           AST
import           Data.Functor.Identity
import           Data.Text.Internal.Lazy
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy.IO          as LT
import           LLVM.AST                   hiding (function, value)
import           LLVM.AST.Type              as AST
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty

compile :: (MonadIRBuilder m) => Expr -> m Operand
-- 整数 n をコンパイルした結果は
compile (Num n) = pure $ int32 n    -- int32 n である
-- e1 + e2 をコンパイルした結果は
compile (BiOp e1 op e2) = do
    e1' <- compile e1
    e2' <- compile e2
    case op of
        "+" -> add e1' e2'
        "-" -> sub e1' e2'
        "*" -> mul e1' e2'
        "/" -> sdiv e1' e2'
        _   -> undefined
