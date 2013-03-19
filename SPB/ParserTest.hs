module SPB.ParserTest where

import qualified Data.Text as T

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Debug.Trace (trace)
import SPB.Base
import SPB.BaseLib
import SPB.Interpreter
import SPB.Parse
import SPB.Javascript
         
readExpr :: String -> IO ()
readExpr input = case parse parseExpr "spb" input of
     Left err -> putStrLn $ "No match: " ++ show err
     Right val -> do 
       liftIO $ putStrLn $ T.unpack $ prettyPrint val
       i <- runInterpM [jsSymbolMap, baseLibSymbolMap] $ evalM val
       case i of
         (Left e) -> putStrLn $ "Error: " ++ show e
         (Right (csexp, symbolMap)) -> putStrLn $ "Program Result: "
                                       ++ T.unpack (prettyPrint csexp)
parserTest = do
  let 
    unsafeProgram = 
      "\"(do (fireMissles 9001) 101010101)\""
      --" \"fireMissles 9001\""
    unsafeProgram2 = 
      "(do (defun afun (x) (lambda (y) x)) ((afun 5) 200))"
    firstSPBProgram = 
        "(do "
        ++ "\n(defun printTwice (arg) (do (print arg) (print arg)))"
        ++ "\n(defun fireMissles (num) (printString (append \"Firing missles: \" (show num))))"
        ++ "\n(defun printSafeEvalResult (res) (do "
        ++ "     (print (car res)) (printString (car (car (cdr res))))))"
        ++ "\n(printTwice 42)"
        ++ "\n(printString \"Hello World!\")"         
        ++ "\n(printString (cdr \"Hello World!\"))"
        ++ "\n(printString (show \"Hello World!\"))"
        ++ "\n(fireMissles 10)"
        ++ "\n(printString \"Unsafe Eval of Unsafe Program 1 Evaluation: \")"
        ++ "\n(print (eval (read "++unsafeProgram++")))"
        ++ "\n(printString \"Safe Eval of Unsafe Program 1 Evaluation: \")"
        ++ "\n(printSafeEvalResult (evalSafe (read "++unsafeProgram++")))"
        ++ "\n(printString \"Unsafe Eval of Unsafe Program 2 Evaluation: \")"
        ++ "\n(print (eval '"++unsafeProgram2++"))"
        ++ "\n(printString \"Safe Eval of Unsafe Program 2 Evaluation: \")"
        ++ "\n(printSafeEvalResult (evalSafe '"++unsafeProgram2++"))"
        ++ "\n(printString (compile-to-javascript '(1 2 3 4 5)))"
        ++ "\n(((\\ (x y) (do (print y) x)) \"xvar\") \"yvar\"))" --Yay currying
  readExpr firstSPBProgram
  error "Parsing complete!"
