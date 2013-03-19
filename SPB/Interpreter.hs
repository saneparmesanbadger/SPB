{-# Language TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses 
 , UndecidableInstances, OverloadedStrings, FlexibleContexts, BangPatterns #-}
module SPB.Interpreter (
  SymbolMap (..)
  , SymbolValue (..)
  , InterpM (..)
  , Interpreter (..)
  , SimState (..)
  , runInterpM
  , evalM
  , __lambda
  , MonadIO
  , S.get
  , S.put
  , liftIO
  , singleError
  , symbolLookup
  , interpError
  , liftBase
  ) where

import qualified Data.Text as T
import Data.Monoid ( (<>) )
import Data.Maybe (isJust)
import qualified Control.Monad.State.Strict as S
import qualified Control.Error as E (EitherT, runEitherT, hoistEither, note, left, right)
import System.Random (StdGen (..), mkStdGen, getStdRandom, randomR)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity)
import qualified Data.HashMap.Strict as HM
import Debug.Trace (trace)

import SPB.Base

type Error = T.Text
             
-- | Default symbolMap contains all builtins: defun, quote, etc
-- We can keep a list of them to simulate a "stack"
type SymbolMap m = HM.HashMap T.Text (SymbolValue m)

data SymbolValue m = 
  Function Int ([CSExp] -> InterpM m CSExp) -- ^ builtins
  | Macro Int ([CSExp] -> InterpM m CSExp) -- ^ quotes lambdas
  | SymbolValue CSExp

instance Show (SymbolValue m) where
  show (Function i f) = "Function of " ++ show i ++ " arguments"
  show (Macro i f) = "Macro of " ++ show i ++ " arguments"
  show (SymbolValue csexp) = "Symbol Value: " ++ show csexp
             

instance Interpreter IO where
  evalI = evalM
  putStrI = liftIO . putStr . T.unpack
  readFileI = liftIO . readFile
  randomI i = liftIO $ getStdRandom (randomR (0,i))
  
instance Interpreter Identity where
  evalI = evalM
  putStrI = return . (const ())
  readFileI = const $ return []
  randomI i = return 0
  
data SimState = SimState { simOutput :: T.Text, simRand :: StdGen }
instance Interpreter (S.StateT SimState Identity) where
  evalI = evalM
  putStrI arg = do
    s <- (liftBase $ S.get)
    (liftBase . S.put) $ s { simOutput = (simOutput s) <> arg }
  readFileI = const $ return []
  randomI i = do  
    s <- (liftBase $ S.get)
    let (r, newGen) = randomR (0, i) (simRand s)
    (liftBase . S.put) $ s { simRand = newGen }
    return r

evalM :: (Monad m, Interpreter m) => 
         CSExp -> InterpM m CSExp
evalM a@(c, SE []) = return a
evalM a@((c, SAtom (ASymbol t))) = do
  s <- symbolLookup c t
  case s of 
    (SymbolValue v) -> return v
    _ -> funcall evalM t []
evalM (c1, SE ((c2, (SAtom (ASymbol t))) : args)) = 
 funcall evalM t args
evalM (c1, SE ((c2, (SAtom l@(ALambda argName b env))) : [])) = do
  stack <- S.get
  S.put $ (HM.map SymbolValue env) : stack
  r <- evalM b
  S.put stack
  return r
evalM (c1, SE ((c2, (SAtom l@(ALambda argName b env))) : args)) =
  runLambdas evalM l args
evalM a@(c1, SE ((c2, (SAtom l@(ALambda argName b env))) : args)) = return a
evalM a@((c, SAtom b)) = return a  
evalM a@(c1, fun@(SE ((c2,b):args))) = do 
  case b of
    (SAtom at) -> singleError c2 $ "Atom (not a function) at head" 
                  <> "of list in: \n\t" <> prettyPrint a
    _ -> do
      z <- evalM (c2,b)
      evalM (c1, SE (z:args))
evalM b = error $ "Don't have a case to eval: " ++ show b

-- ^ Recursively run a lambda on the arguments that follow it,
-- Until it no longer returns a lambda, or there are no arguments left.
runLambdas :: (Monad m, Interpreter m) =>
           (CSExp -> InterpM m CSExp) -- ^ Which eval to use
           -> Atom
           -> [CSExp] -- ^ Symbol Values
           -> InterpM m CSExp       
runLambdas eval l@(ALambda argName body env0) args = do
  (c, argVal) <- evalI $ head args
  lamVal <- runLambda eval l (c, argVal)
  case lamVal of
    (c, (SAtom l'@(ALambda a b env))) -> 
      case length args > 0 of
        True -> runLambdas eval l' (tail args)
        False -> return lamVal
    b -> do
      return b
runLambdas _ _ _ = interpError ["runLambdas called on non-lambda"]


-- ^ Order of symbol names should be REVERSED before being sent to __lambda
__lambda :: [T.Text] -> CSExp -> CSExp
__lambda [] body = body
__lambda (t:ts) (c,body) = __lambda ts (c, SAtom (ALambda t (c,body) HM.empty))


runLambda :: (Monad m) => 
           (CSExp -> InterpM m CSExp) -- ^ Which eval to use
           -> Atom 
           -> CSExp  -- ^Symbol value for argument
           -> InterpM m CSExp       
runLambda eval (ALambda symName body env) !symVal  = do
  stack <- S.get
  let s'0 =  HM.fromList [(symName, symVal)]
  S.put $ (HM.map SymbolValue s'0) : (HM.map SymbolValue env) : stack
  res <- eval body 
  let res' = case res of 
        (c, SAtom (ALambda symName' body' env')) ->
          (c, SAtom (ALambda symName' body' ((HM.union env (HM.union env' s'0)))))
        b -> b
  S.put stack
  return res'

  
funcall :: (S.Monad m, Monad m, Interpreter m) =>  --, Interpreter (InterpM m CSExp)) => 
           (CSExp -> InterpM m CSExp) -- ^ Which eval to use
           -> T.Text 
           -> [CSExp]
           -> InterpM m CSExp       
funcall eval t args = do
  let ca = Context 0
  f <- symbolLookup ca t
  case f of
    (SymbolValue s) -> 
      case s of
        ( (c, SAtom l@(ALambda t f env))) -> runLambdas eval l args
        _ -> 
          case length args > 0 of
            True -> do
              evaledBody <- evalM s
              evalM $ c0 $ SE $ evaledBody:args
            False -> evalM s
    (Macro nargs f) -> do 
      case nargs == length args of
        True -> f args
        False -> 
          case nargs == -1 of
            True -> f args
            False -> 
              case length args < nargs of
                True -> singleError ca $ "Currying library macros" 
                        <> " not yet implemented (on macro " <> t <>")"
                False -> singleError ca $ "Macro " <> t <> " takes " 
                         <> T.pack (show nargs) <> "args, "
                         <> T.pack (show (length args)) <> "given."
    (Function nargs f) -> do
      case nargs == length args of
        True -> f =<< mapM eval args
        False -> 
          case nargs == -1 of
            True -> f =<< mapM eval args
            False -> 
              case length args < nargs of
                True -> singleError ca $ "Currying library functions"
                        <> " not yet implemented (on fn " <> t <>")"
                False -> singleError ca $ "Macro " <> t <> " takes " 
                         <> T.pack (show nargs) <> "args, "
                         <> T.pack (show (length args)) <> "given."
                 
args2Map :: e -> [CSExp] -> SymbolMap m
args2Map e as = HM.empty
          
singleError :: (Monad m) => Context -> T.Text -> InterpM m a
singleError c t = interpError [lineError c <> ": " <> t]
     
lineError :: Context -> T.Text
lineError c = "Error on line " <> T.pack (show $ lineNumber c)
     
-- ^ Awesome inefficient symbol lookup! Linear in the stack depth! Yay!
symbolLookup :: (Monad m) => Context -> T.Text -> InterpM m (SymbolValue m)
symbolLookup c t = do
  stack <- S.get
  let founds = filter isJust $ map (HM.lookup t) stack
  case founds of
    ((Just f):fs) -> return f
    [] -> singleError c $ ": Symbol " <> t <> " not defined"

-- Create a nice monad stack so we can deal with errors and keep state etc etc
type Interp m =  S.StateT ([SymbolMap m]) (E.EitherT [Error] m)

class Interpreter m where
  evalI :: CSExp -> InterpM m CSExp
  putStrI :: T.Text -> InterpM m ()
  randomI :: Int -> InterpM m Int
  readFileI :: String -> InterpM m String
  
newtype InterpM m a = InterpM { unInterp :: S.StateT ([SymbolMap m]) (E.EitherT [Error] m) a }

instance (Monad m) => Monad (InterpM  m) where
  return a = InterpM $ return a
  m >>= k = InterpM $ do
    a <- unInterp m
    unInterp $ k a

instance (Monad m) => S.MonadState [SymbolMap m] (InterpM m) where
  get = InterpM $ S.get
  put = InterpM . S.put
  
instance (MonadIO m) => MonadIO (InterpM m) where
  liftIO = InterpM . lift . liftIO 
  
liftBase :: (Monad m) => m a -> InterpM m a
liftBase = InterpM . lift . lift 
  
runInterpM :: (Monad m) =>   
              [SymbolMap m]-- ^ Beginning state
              -> InterpM m a
              -> m (Either [Error] (a, [SymbolMap m]))
runInterpM s x = E.runEitherT $ S.runStateT (unInterp x) s

interpError :: (Monad m) => [T.Text] -> InterpM m a
interpError = InterpM . lift . E.left
  