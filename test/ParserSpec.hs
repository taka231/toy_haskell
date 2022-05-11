module ParserSpec (spec) where
import           AST
import           Parser
import           Test.Hspec

spec :: Spec
spec = do
    describe "parse +" $
        it "parse 1 + 1" $
            parseExpr "1 + 1" `shouldBe` BiOp (Num 1) "+" (Num 1)
    describe "parse -" $
        it "parse 13 - 1" $
            parseExpr "13 - 1" `shouldBe` BiOp (Num 13) "-" (Num 1)
