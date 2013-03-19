{-# Language OverloadedStrings #-}
module SPB.Test where
import qualified Data.HashMap.Strict as HM

import SPB.Base
import SPB.BaseLib
import SPB.Interpreter

testFuncall s r = testList [testAtom (ASymbol s), r]
testAtom a = c0 $ SAtom a
testList contents = c0 $ SE contents

testExp = 
{-  testFuncall "printString" $ testFuncall "show" $ testFuncall "quote" $ 
  testFuncall "print" $ testAtom (AInt 42) -}
  testList [ testAtom (ASymbol "do")
           ,(testList [testAtom (ASymbol "defun")
                      , testAtom (ASymbol "funprime")
                      ,testList [testAtom (ASymbol "arg1"), testAtom (ASymbol "arg2")]
                      ,testList [ testAtom (ASymbol "do") 
                                ,(testFuncall "print" (testAtom $ ASymbol "arg1"))  
                                ,(testFuncall "print" (testAtom $ ASymbol "arg2"))
                                  ]])
           ,testList [testAtom (ASymbol "funprime"), (testAtom $ AInt 1), (testAtom $ AInt 2)]
           ,testList [ testAtom (ALambda "myArg1" (testFuncall "print" 
                                                   (testAtom $ ASymbol "myArg1")) HM.empty)
                     , testAtom (AInt 424242)]]
  

interpreterTest = do
  i <- runInterpM [baseLibSymbolMap] $ evalM testExp
  putStrLn $ show i
  error "Intended premature termination"
