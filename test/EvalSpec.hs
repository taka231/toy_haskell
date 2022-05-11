module EvalSpec (spec) where

import           AST
import           Eval
import           Test.Hspec

spec :: Spec
spec = do
    describe "eval +" $
        it "eval +" $
            eval (BiOp (Num 1) "+" (Num 1)) `shouldBe` pure (VNum 2)
    describe "eval -" $
        it "eval -" $
            eval (BiOp (Num 1) "-" (Num 1)) `shouldBe` pure (VNum 0)
    describe "eval *" $
        it "eval *" $
            eval (BiOp (Num 2) "*" (Num 3)) `shouldBe` pure (VNum 6)
    describe "eval /" $
        it "eval /" $
            eval (BiOp (Num 4) "/" (Num 2)) `shouldBe` pure (VNum 2)
    describe "eval ==" $
        it "eval == TNum" $
            eval (BiOp (Num 4) "==" (Num 4)) `shouldBe` pure (VBool True)
    describe "eval ==" $
        it "eval == TBool" $
            eval (BiOp (Bool True) "==" (Bool True)) `shouldBe` pure (VBool True)
    describe "eval if" $
        it "eval if then" $
            eval (If (Bool True) (Num 1) (Num 2)) `shouldBe` pure (VNum 1)
    describe "eval if" $
        it "eval if else" $
            eval (If (Bool False) (Num 1) (Num 2)) `shouldBe` pure (VNum 2)
