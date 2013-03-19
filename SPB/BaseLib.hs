{-# Language OverloadedStrings, ScopedTypeVariables #-}
module SPB.BaseLib where

import Data.Monoid ( (<>) )
import SPB.Base
import SPB.Interpreter
import SPB.Parse
import SPB.Javascript
import System.Random (mkStdGen)
import Control.Monad.Identity (Identity, runIdentity)
import qualified Control.Monad.State.Strict as S
import Text.ParserCombinators.Parsec (parse)
import Safe
import Debug.Trace (trace)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.List (intercalate)

baseLibSymbolMap :: (Monad m, Interpreter m) => SymbolMap m
baseLibSymbolMap = HM.fromList [
  ("T", SymbolValue (c0 (SAtom T)))
  ,("NIL", SymbolValue (c0 (SE [])))
  ,("quote", Macro 1 bl_quote)
  ,("defun", Macro 3 bl_defun)
  ,("lambda", Macro 2 bl_lambda)
  ,("do", Macro (-1) bl_do)
  ,("let", Macro (-1) bl_let)
  ,("car", Function 1 bl_car)
  ,("first", Function 1 bl_car)
  ,("cdr", Function 1 bl_cdr)
  ,("rest", Function 1 bl_cdr)
  ,("nth", Function 2 bl_nth)
  ,("length", Function 1 bl_length)
  ,("cons", Function 2 bl_cons)
  ,("append", Function 2 bl_append)
  ,("if", Macro (-1) bl_if)
  ,("and", Function 2 bl_and)
  ,("or", Function 2 bl_or)
  ,("+", Function 2 $ _bl_arithOp (+))
  ,("-", Function 2 $ _bl_arithOp (-))
  ,("*", Function 2 $ _bl_arithOp (*))
  ,("/", Function 2 $ _bl_arithOp (/))
  ,("<", Function 2 $ _bl_compOp (<))
  ,(">", Function 2 $ _bl_compOp (>))
  ,("<=", Function 2 $ _bl_compOp (<=))
  ,(">=", Function 2 $ _bl_compOp (>=))
  ,("==", Function 2 $ _bl_equal)
  ,("not", Function 1 $ _bl_not)
  ,("make-hashtable", Macro 1 bl_make_hashtable)
  ,("hashtable-set", Macro 3 bl_hashtable_set)
  ,("hashtable-get", Macro 2 bl_hashtable_get)
  ,("random", Function 1 bl_random)
  ,("load", Function 1 bl_load)
  ,("eval", Function 1 bl_eval)
  ,("read", Function 1 bl_read)
  ,("readMay", Function 1 bl_readMay)
  ,("evalSafe", Function 1 bl_evalSafe)
  ,("print", Function 1 bl_print)
  ,("show", Function 1 bl_show)
  ,("printString", Function 1 bl_printString)
  ]


-- | Quotes code so it doesn't get evaluated
bl_quote :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_quote (exp:[]) = return exp

bl_defun :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_defun (name:args@(c, SE argsi):body:[]) = do
  n <- getSymbol name
  tArgs <- case length argsi > 0 of
    True -> mapM getSymbol =<< getList args
    False -> return []
  s <- get
  let newTop = HM.insert n (SymbolValue $ __lambda (reverse tArgs) body) (head s)
  put $ newTop : (tail s)
  return $ c0 (SAtom T)
bl_defun ((c, a):b) = singleError c "Invalid defun."
  
bl_lambda :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_lambda (args:body:[]) = do
  tArgs <- mapM getSymbol =<< getList args
  return $ __lambda (reverse tArgs) body
  
-- ^ progn with a (better?) name
bl_do :: (Monad m, Interpreter m) => 
         [CSExp] 
         -> InterpM m CSExp
bl_do [] = interpError ["(do) called without any parameters"]
bl_do exps = do
  mapM_ evalI $ init exps
  z <- evalI $ last exps
  return z


bl_if :: (Monad m, Interpreter m) => 
         [CSExp] 
         -> InterpM m CSExp
bl_if exps = _bl_if exps

_bl_if :: (Monad m, Interpreter m) => 
         [CSExp] 
         -> InterpM m CSExp
_bl_if [] = return $ c0  $ SE []
_bl_if (exp:[]) = evalI exp
_bl_if (exp1:exp2:exps) = do
  evd <- (evalI exp1)
  _if evd exp2 exps
  where
    _if (c, SE []) exp2 exps = _bl_if exps
    _if (c, _) exp2 exps = evalI exp2
  
bl_and :: (Monad m, Interpreter m) => 
         [CSExp] 
         -> InterpM m CSExp
bl_and [] = return $ c0  $ SE []
bl_and ((c, SE []):exp2:[]) = return $ c0  $ SE []
bl_and (exp1:(c, SE []):[]) = return $ c0  $ SE []
bl_and (exp1:exp2:[]) = return $ c0 $ SAtom T

bl_or :: (Monad m, Interpreter m) => 
         [CSExp] 
         -> InterpM m CSExp
bl_or [] = return $ c0  $ SE []
bl_or ((c, SE []):(c2, SE []):[]) = return $ c0  $ SE []
bl_or (exp1:exp2:[]) = return $ c0 $ SAtom T

bl_let :: (Monad m, Interpreter m) => 
         [CSExp] 
         -> InterpM m CSExp
bl_let [] = return $ c0  $ SE []
bl_let (exp:[]) = evalI exp
bl_let (exp1:exp2:exps) = do
  s <- getSymbol exp1
  stack <- get
  evd <- (evalI exp2)
  put $ (HM.insert s (SymbolValue evd) HM.empty) : stack
  z <- bl_let exps
  put stack
  return z

_bl_arithOp :: (Monad m, Interpreter m) => 
               (Double -> Double -> Double)
                     -> [CSExp] 
                     -> InterpM m CSExp
_bl_arithOp _ [] = interpError ["Arithmetic operator takes 2 arguments."]
_bl_arithOp _ (exp:[]) = interpError ["Arithmetic operator takes 2 arguments."]
_bl_arithOp f (exp:exps) = do
  num <- numFromExp exp
  nums <- mapM numFromExp exps
  return . c0 . SAtom . (returnType (exp:exps)) $ foldl f num nums
  where
    returnType [] = AInt . floor
    returnType ((c, (SAtom (AInt _))):exps) = returnType exps
    returnType b = AFloating

numFromExp (c, (SAtom (AInt i))) = return $ fromIntegral i
numFromExp (c, (SAtom (AFloating f))) = return f
numFromExp (c, exp) = singleError c $ "Expression " 
                      <> prettyPrint (c0 exp) <> " is not a number"

_bl_equal :: (Monad m, Interpreter m) => 
             [CSExp] 
             -> InterpM m CSExp
_bl_equal (exp1:exp2:[]) =              
  return $ if (exp1 == exp2) then (c0 $ SAtom T) else (c0 $ SE [])
                                                      
_bl_not :: (Monad m, Interpreter m) => 
             [CSExp] 
             -> InterpM m CSExp
_bl_not (exp1:[]) =              
  return . c0 $ case exp1 of
    (c, SE []) -> SAtom T
    z -> SE []

_bl_compOp :: (Monad m, Interpreter m) => 
               (Double -> Double -> Bool)
                     -> [CSExp] 
                     -> InterpM m CSExp
_bl_compOp f (exp1:exp2:[]) = do
  num1 <- numFromExp exp1
  num2 <- numFromExp exp2
  return $ c0 $ case f num1 num2 of
    True -> SAtom T
    False -> SE []
    
bl_make_hashtable :: (Monad m, Interpreter m) => 
                      [CSExp] 
                      -> InterpM m CSExp
bl_make_hashtable (n:[]) = do                      
  name <- getSymbol n
  (s:stack) <- get
  put $ HM.insert name (SymbolValue . c0 . SAtom . AHashMap $ HM.empty) s : stack
  return (c0 $ SAtom T)
  
bl_hashtable_set :: (Monad m, Interpreter m) => 
                [CSExp] 
                -> InterpM m CSExp
bl_hashtable_set (n:k:v:[]) = do                      
  name <- getSymbol n
  k' <- evalI k
  v' <- evalI v
  stack <- get
  mutateHashTable name k' v' [] stack
  where
    mutateHashTable n k v prev [] = return . c0 . SE $ []
    mutateHashTable n k v prev (s:stack) =
      case HM.lookup n s of 
        Nothing -> mutateHashTable n k v (s:prev) stack
        (Just (SymbolValue (c, SAtom (AHashMap something)))) -> do
          put $ (reverse prev) ++ [HM.insert n (SymbolValue . c0 . SAtom 
                                              . AHashMap $ HM.insert (snd k) v something) s] 
            ++ stack
          return (c0 $ SAtom T)
        _ -> interpError ["Symbol " <> n <> " is not a hashtable"]

bl_hashtable_get :: (Monad m, Interpreter m) => 
                [CSExp] 
                -> InterpM m CSExp
bl_hashtable_get (n:k:[]) = do                      
  name <- getSymbol n
  k' <- evalI k
  hm <- symbolLookup (fst n) name
  case hm of 
    (SymbolValue (c, SAtom (AHashMap (hm')))) -> 
      case HM.lookup (snd k') hm' of
        (Just v) -> return v
        Nothing -> return (c0 $ SE [])
    _ -> return (c0 $ SE [])

-- | Prints an s-expression presumed to be a list of chars
bl_read :: (Monad m, Interpreter m) => [CSExp] -> InterpM m CSExp 
bl_read ( (c, SE exps) : []) =  _read (map extractChar exps)
bl_read _ = return $ c0 (SE [])        

-- | returns a list: [Result, Errors]
bl_readMay :: (Monad m, Interpreter m) => [CSExp] -> InterpM m CSExp 
bl_readMay ( (c, SE exps) : []) = return $ case parse parseExpr "spb" (map extractChar exps) of
  Left err -> c0 $ SE [c0 $ SE [], c0 $ SE (map (c0 . SAtom . AChar) (show err))]
  Right val -> c0 $ SE [val, c0 $ SE []]
bl_readMay _ = return $ c0 $ SE [c0 (SE []), c0 . SE $ map (c0 . SAtom . AChar) 
                                        "(readMay) should be passed a string"]

{-
extractChar (_, (SAtom (AChar c))) = c
extractChar _ = ' '
-}
_read :: (Monad m) => String -> InterpM m CSExp
_read str = 
  return $ case parse parseExpr "spb" str of
    Left err -> c0 $ SE []
    Right val -> val

bl_random :: (Monad m, Interpreter m) => [CSExp] -> InterpM m CSExp 
bl_random (s@(c, SAtom (AInt i)):[]) = do
  r <- randomI i
  return . c0 . SAtom . AInt $ r
bl_random ((c, _):_) = singleError c "Random requires a single integer argument"


bl_load :: (Monad m, Interpreter m) => [CSExp] -> InterpM m CSExp 
bl_load (s@(c, SE chars):[]) = do
  z <- readFileI (map extractChar chars)
  res <- _read z
  evalI res
bl_load _ = interpError ["Load requires the filename as a string"]

bl_eval :: (Monad m, Interpreter m) => [CSExp] -> InterpM m CSExp 
bl_eval (exp:[]) = evalI exp

bl_evalSafe :: (Monad m, Interpreter m) => [CSExp] -> InterpM m CSExp 
bl_evalSafe (exp@(c,exp'):[]) = do
  seed <- randomI 4000000000
  let 
    ires = runInterpM [jsSymbolMap, baseLibSymbolMap] $ evalI $ exp
    sres =  runIdentity $ S.runStateT ires $ SimState "" $ mkStdGen seed
    res = fst sres
    state = snd sres
  case res of 
    (Left es) -> return
                 (c, SE [(c, (SE []))
                          ,(c, SE (map ((string2sexp (SE [])) . T.unpack) es))
                          ,(c, (SE []))])
                 --show (head es)) (fst exp, (SE []))
    (Right res) -> return (c, SE [fst res, (c, (SE [])), (c, (SE outputStr))])
      where
        outputStr = map (c0 . SAtom . AChar) $ T.unpack $ simOutput state

{-
-- | Alter the code to run inside a different base monad
-- Attempts to preserve lambda closures etc,
-- But will erase all builtins contained within its environment.
stripMap :: CSExp -> CSExp
stripMap (c, (SAtom (ALambda t body env))) = 
  (c, SAtom (ALambda t (stripMap body) newEnv))
  where
    newEnv = (HM.foldlWithKey' alter HM.empty env)
    alter hm k (SymbolValue v) = HM.insert k (SymbolValue (stripMap v)) hm 
    alter hm k _ = hm
stripMap (c, (SE [])) = (c, (SE []))
stripMap (c, SE cs) = (c, SE (map (stripMap) cs))
stripMap (c, (SAtom (ASymbol a))) = (c, SAtom (ASymbol a))
stripMap (c, (SAtom (ABool a))) = (c, SAtom (ABool a))
stripMap (c, (SAtom (AChar a))) = (c, SAtom (AChar a))
stripMap (c, (SAtom (AInt a))) = (c, SAtom (AInt a))
stripMap (c, (SAtom (AFloating a))) = (c, SAtom (AFloating a))
-}
bl_car :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_car ((c, (SE [])):[]) = return (c, (SE []))
bl_car (l:[]) = do
  tArgs <- getList l
  case tArgs of
    [] ->  return (fst l, (SE []))
    (x:xs) -> return x
    
bl_cdr :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_cdr ((c, (SE [])):[]) = return (c, (SE []))
bl_cdr (l:[]) = do
  tArgs <- getList l
  case tArgs of
    [] ->  return (fst l, (SE [])) 
    (x:xs) -> return (fst l, SE xs)
    
bl_length :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_length ((c, (SE l)):[]) = return $ (c, SAtom $ AInt (length l))
bl_length ((c,l):[]) = singleError c "Length must be called on a list"

bl_nth :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_nth ((c0, SAtom (AInt i)):(c, (SE l)):[]) = return $ atDef (c, SE []) l i

bl_cons :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_cons (a:(c2, SE b):[]) = return (c2, (SE (a:b)))
bl_cons ((c, a):b) = singleError c "Second argument to cons must be a list."
    
bl_append :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_append ((c, (SE [])):l2:[]) = return l2
bl_append (l1:(c, (SE [])):[]) = return l1
bl_append (l1:l2:[]) = do
  a <- getList l1
  b <- getList l2
  return $ (fst l1, SE (a++b))

getSymbol s = case s of
  (c, (SAtom (ASymbol sym))) -> return sym
  (c, exp) -> singleError c $ "Expected symbol, got " <> prettyPrint (c, exp)
  
getList l = case l of  
  (c, SE a) -> return a
  (c, exp) -> singleError c $ "Expected list, got " <> prettyPrint (c, exp)
  
-- | Prints code in a standard way.
bl_print :: (Monad m, Interpreter m) => [CSExp] -> InterpM m CSExp 
bl_print (exp:[]) = do
  putStrI $ prettyPrint exp <> "\n"
  return $ (Context 0, (SE []))

-- | Prints an s-expression presumed to be a list of chars
bl_printString :: (Monad m, Interpreter m) => [CSExp] -> InterpM m CSExp  
bl_printString ( (_, SE exps) : []) = do
  putStrI $ T.pack $ (map extractChar exps) ++ "\n"
  return $ c0 (SAtom T)
  where
    extractChar (_, (SAtom (AChar c))) = c
    extractChar _ = ' '
bl_printString _ = return $ c0 (SAtom T)

-- | Returns a string representation of code  
bl_show :: (Monad m) => [CSExp] -> InterpM m CSExp
bl_show (exp:[]) = do
  return (Context 0, SE (map (c0 . SAtom . AChar) $ T.unpack $ prettyPrint exp))
