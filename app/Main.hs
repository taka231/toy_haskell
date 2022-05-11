module Main        where
import           AST
import           Compile
import           Control.Monad     (unless)
import qualified Data.Map          as Map
import qualified Data.Text.Lazy.IO as LT
import           Eval
import           LLVM.AST.Type
import           LLVM.IRBuilder
import           LLVM.Pretty
import           Parser
import           System.IO

-- main :: IO ()
-- main = do
--     input <- read_
--     unless (input == ":q") $ print_ (eval_ input) >> main
--
-- read_ :: IO String
-- read_ = putStr "input?> " >> hFlush stdout >> getLine
--
-- eval_ :: String -> String
-- eval_ input = case runEval Map.empty $ parseExpr input of
--     Right value -> show value
--     Left err    -> show err
--
-- print_ :: String -> IO ()
-- print_ = putStrLn
--

main :: IO ()
main = do
    n <- getContents
    let ast = parseExpr n                -- 抽象構文木をつくり
    LT.putStrLn $                          -- 以下のLLVM IR を出力する
        ppllvm $ buildModule "main" $ do   -- main モジュールのなかに
        function "main" [] i32 $ \[] -> do -- main 関数があり、
            opr <- compile ast             -- 式をコンパイルして
            ret opr
