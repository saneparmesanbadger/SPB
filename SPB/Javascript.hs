{-# LANGUAGE OverloadedStrings #-}
module SPB.Javascript where

import Data.Monoid ((<>), mconcat)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Control.Monad.State.Strict 
import SPB.Interpreter
import SPB.Base

type RenameMap = (Int, HM.HashMap T.Text [(Bool, Int)], HM.HashMap T.Text T.Text)

jsSymbolMap :: (Monad m) => SymbolMap m
jsSymbolMap = HM.fromList [
  ("compile-to-javascript", Macro 1 js_compile)
  ,("json", Function 1 js_json)
  ]

renameMap = HM.fromList [ 
  ("quote", "_$quote")
  ,("defun", "_$defun")
  ,("car", "_$car") 
  ,("first", "_$car") 
  ,("cdr", "_$cdr") 
  ,("rest", "_$rest") 
  ,("append", "_$append") 
  ,("print", "_$print")
  ,("show", "_$show")
  ,("printString", "_$printString")
  ,("+", "_$add")
  ,("-", "_$sub")
  ,("*", "_$mul")
  ,("/", "_$div")
  ,(">", "_$gt")
  ,("<", "_$lt")
  ,("==", "_$eq")
  ,("not", "_$not")
  ,("hashtable-set", "_$objSet")
  ,("hashtable-get", "_$objGet")
  ,("make-hashtable", "_$newObj")
  ]


js_compile :: (Monad m) => [CSExp] -> InterpM m CSExp
js_compile (exp:[]) = do
  s <- get
  return $ string2sexp (SE []) $ T.unpack $ (flip evalState) (0, HM.empty, renameMap) $ do
    s1 <- jsCompileSymbolMap exp $ purifyMap s
    s2 <- sexp2JS exp
    return $ s1 <> s2
    
js_json :: (Monad m) => [CSExp] -> InterpM m CSExp
js_json (exp:[]) = do
  s <- get
  return $ string2sexp (SE []) $ T.unpack $ jsonExp exp

      

jsonExp (c, SE []) = "[]"
jsonExp (c, SE ((c2,(SAtom (AChar ch))):chs)) =
  "\"" <> T.singleton ch <> T.pack (map extractChar chs) <> "\""
jsonExp (c, SE ((c2,(SAtom (ASymbol "quote"))):t:[])) = jsonExp t
jsonExp (c, SE xs) =
 "[" <> T.intercalate ", " (map jsonExp xs) <> "]"
jsonExp z@(c, SAtom (AHashMap hm)) = 
  "{" <> HM.foldlWithKey' (\a k v -> 
                            let sep = if (T.length a > 0) then ", " else ""
                            in a <> sep <> jsonExp (c0 k) <> " : " <> jsonExp v) "" hm <> "}"
jsonExp z@(c, SAtom a) = prettyPrint z
jsonExp _ = ""
  
declare :: T.Text -> CSExp -> State RenameMap T.Text
declare k v = do
  --withRename k $ \rk -> do
  rk <- renamePush False k
  rv <- sexp2JS v
  return $ "\nvar " <> rk <> " = " <> rv <> ";"


extractSymbol s = case s of
  (c, (SAtom (ASymbol sym))) -> sym
  (c, exp) -> "" --singleError c $ "Expected symbol, got " <> prettyPrint (c, exp)
  
extractList l = case l of  
  (c, SE a) -> a
  (c, exp) -> [] 

extractChar (_, (SAtom (AChar c))) = c
extractChar _ = ' '

sexp2JS :: CSExp -> State RenameMap T.Text
sexp2JS (c, SE []) = return "null"
sexp2JS (c, SE ((c2,(SAtom (ASymbol "lambda"))):args:body:rest)) = do
  sexp2JS $ __lambda (reverse $ map extractSymbol $ extractList args) body
sexp2JS (c, SE ((c2,(SAtom (ASymbol "quote"))):t:[])) = sexp2JS t
sexp2JS (c, SE ((c2,(SAtom (ASymbol t))):[])) = do
  rt <- getRename t
  return $ rt <> "()"; 
sexp2JS (c, SE ((c2,(SAtom (ASymbol "do"))):xs)) = do
  rxs <- mapM sexp2JS xs
  return $ "(function(){ " <> T.intercalate "; " (init rxs) 
    <> "; return " <> last rxs <> ";}())";
sexp2JS (c, SE ((c2,(SAtom (ASymbol "let"))):xs)) = do
  z <- _jsLet xs
  return $ "function(){" <> z <> "}();\n"
  where
    _jsLet [] = return "null"
    _jsLet (x1:[]) = fmap (\x -> "return " <> x) $ sexp2JS x1
    _jsLet ((c1, (SAtom (ASymbol t))):x2:x3) =
      withRename t $ \rt -> do
        val <- sexp2JS x2
        rest <- _jsLet x3
        return $ "var " <> rt <> " = " <> val <> ";\n " <> rest
sexp2JS (c, SE ((c2,(SAtom (ASymbol "if"))):xs)) = do
  z <- _jsIf xs
  return $ "function(){" <> z <> "}();\n"
  where
    _jsIf [] = return "null"
    _jsIf (x1:[]) = fmap (\x -> "return " <> x) $ sexp2JS x1
    _jsIf (x1:x2:x3) = do
      x1' <- sexp2JS x1
      x2' <- sexp2JS x2
      x3' <- _jsIf x3
      return $ "if (" <> x1' <> "){ " <> x2' <> "} else { " <> x3' <> " }"
sexp2JS (c, SE ((c2,(SAtom (ASymbol t))):xs)) = do
  rxs <- mapM sexp2JS xs
  case HM.lookup t renameMap of
    -- Special case for builtin calls since _$apply won't work correctly
    (Just libFunName) ->
      return $ libFunName <> "(" <>  T.intercalate ", " rxs  <> ")"
    Nothing -> do
      rt <- getRename t
      return $ "_$apply("<> rt <> ", " <>  "[" <> T.intercalate ", " rxs <> "]" <> ")"
sexp2JS (c, SE ((c2,(SAtom (AChar ch))):chs)) =
  return $ "\"" <> T.singleton ch <> T.pack (map extractChar chs) <> "\""  
sexp2JS (c, SE ((c2,(SAtom (ASymbol "JSFFI"))):chs)) =
  return $ T.pack (map extractChar chs) 
sexp2JS (c, SE xs) = do
  rxs <- mapM sexp2JS xs
  return $ "[" <> T.intercalate ", " rxs <> "]"
sexp2JS z@(c, SAtom T) = return "true"
sexp2JS z@(c, SAtom (AHashMap hm)) = do
  r <- HM.foldlWithKey' (\a k v -> 
                          do
                            k' <- sexp2JS (c0 k)
                            v' <- sexp2JS v
                            a' <- a
                            let sep = if (T.length a' > 0) then ", " else ""
                            return $ a' <> sep <> k' <> " : " <> v') (return "") hm
  return $ "{" <> r <> "}"              
sexp2JS (c, (SAtom (ASymbol t))) = getRename t
sexp2JS (c, (SAtom (AChar ch))) = return $ "'" <> T.singleton ch <> "'"
sexp2JS a@(c, (SAtom (ALambda t body env))) = do
  renv <- jsCompileSymbolMap a [env]
  withRename t $ \rt -> do
    rbody <- sexp2JS body
    return $ renv <> "function(" <> rt <> "){ return " <> rbody <> "; }\n"
sexp2JS a@(c, (SAtom b)) = return $ prettyPrint a

purifyMap :: (Monad m) => 
             [SymbolMap m]
             -> [HM.HashMap T.Text CSExp]
purifyMap symMap = 
  map (HM.foldlWithKey' f HM.empty) $ symMap
  where
    f m k (SymbolValue v) = HM.insert k v m
    f m k _ = m

jsCompileSymbolMap :: CSExp 
                      -> [HM.HashMap T.Text CSExp]
                      -> State RenameMap T.Text  
jsCompileSymbolMap _ symMap = 
  fmap mconcat $ mapM (HM.foldlWithKey' f (return "")) symMap
  where
    f s k v = do
      s' <- s 
      d <- declare k v
      return $ s' <> d

int2ID :: Int -> T.Text
int2ID i = T.pack $ "a$" ++ show i

renamePush :: Bool -> T.Text -> State RenameMap T.Text
renamePush open name = do
  (s1, s2, s3) <- get
  let next = s1 + 1
  let stack = fromMaybe [] $ HM.lookup name s2
  case stack of
    ((True, i):stck) -> do
      put $ (next, HM.insert name ((open, i):tail stack) s2, s3)
      return $ int2ID i
    _ -> do
      put $ (next, HM.insert name ((open, next):stack) s2, s3)
      return $ int2ID next

renamePop :: T.Text -> State RenameMap ()
renamePop name = do 
  (s1,s2,s3) <- get
  let stack = fromMaybe [] $ HM.lookup name s2
  put $ (s1, HM.insert name (tail stack) s2, s3) 
  -- fst s - 1 presumes pushes/pops symmetric or that it doesn't matter due to nesting
  
getRename :: T.Text -> State RenameMap T.Text
getRename name = do 
  (s1, s2, s3) <- get
  case HM.lookup name s2 of
    (Just stack) -> return $ int2ID (snd $ head stack)
    Nothing -> case HM.lookup name s3 of
      (Just r) -> return r
      Nothing -> renamePush True name 
    
withRenames :: [T.Text] -> ([T.Text] -> State RenameMap a) -> State RenameMap a
withRenames ts f = do
  renamed <- mapM (renamePush False) ts
  res <- f renamed
  mapM renamePop ts
  return res

withRename :: T.Text -> (T.Text -> State RenameMap a) -> State RenameMap a
withRename ts f = do
  renamed <- renamePush False ts
  res <- f renamed
  renamePop ts
  return res
