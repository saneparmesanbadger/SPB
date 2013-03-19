{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module SPB.SPBWebLib where

import Data.Maybe (fromMaybe)
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.Time.Clock (getCurrentTime, addUTCTime, UTCTime (..))
import Control.Concurrent.STM (atomically, readTVar, modifyTVar)
import System.Random (getStdRandom, randomR)
import SPB.Base
import SPB.BaseLib (_read)
import SPB.Interpreter
import SPB.Parse
import SPB.Server.Env
import SPB.Server.HTTP
import Network.URL (decString, encString)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Strict as S
import qualified Data.HashMap.Strict as HM
import Debug.Trace (trace)

spbWebSymbolMap :: Env -> Request -> SymbolMap (S.StateT SPBState IO)
spbWebSymbolMap env req = HM.fromList [
  ("get-post", Function 1 (spb_get_post req))
  ,("set-cookie", Function 2 (spb_set_cookie))
  ,("get-cookie", Function 1 (spb_get_cookie))
  ,("set-memory", Function 2 (spb_set_memory))
  ,("get-memory", Function 1 (spb_get_memory))
  ,("set-session", Function 2 (spb_set_session))
  ,("get-session", Function 1 (spb_get_session))
  ]

data SPBState = SPBState {
  spbCookies :: [Cookie]
  ,spbEnv :: Env
  ,spbOutput :: T.Text
  }
                
type SPBM = S.StateT SPBState IO

instance Interpreter (S.StateT SPBState IO) where
  evalI = evalM
  putStrI str = do
    s <- (liftBase $ S.get)
    (liftBase . S.put) $ s { spbOutput = spbOutput s <> str }
  readFileI = liftIO . readFile
  randomI i = liftIO $ getStdRandom (randomR (0,i))
  
  
getSymbol s = case s of
  (c, (SAtom (ASymbol sym))) -> return sym
  (c, exp) -> singleError c $ "Expected symbol, got " <> prettyPrint (c, exp)
  
extractChar (_, (SAtom (AChar c))) = c
extractChar _ = ' '

spb_get_post :: (Monad m, Interpreter m) => 
                Request
                -> [CSExp] 
                -> InterpM m CSExp
spb_get_post req ((c, SE s):[]) = do                      
  let k = map extractChar s
  trace ("PostData: " ++ show s ++ " (" ++ k ++ "): "++ show (postData req)) $ case HM.lookup (C.pack k) (postData req) of
    Nothing -> return . c0 $ SE []
    (Just v) -> return . c0 . SE $ map (c0 . SAtom . AChar) (C.unpack v)

spb_set_cookie ::
  [CSExp] 
  -> InterpM SPBM CSExp
spb_set_cookie (k:v:[]) = do
  s <- liftBase S.get
  cook <- liftIO $ simpleCookie (t k) (t v) 3600
  (liftBase . S.put) $ s { spbCookies = cook : (spbCookies s) }
  return $ c0 . SAtom $ T
  where t = C.pack . encodeCookieStr . T.unpack . prettyPrint
        
spb_get_cookie ::
  [CSExp] 
  -> InterpM SPBM CSExp
spb_get_cookie (k:[]) = do
  let tk = t k
  s <- liftBase S.get
  trace ("Searching for " ++ C.unpack tk ++ " in " ++ show (spbCookies s)) $
    case searchCookies tk (spbCookies s) of
      Nothing -> return . c0 . SE $ []
      (Just c) -> return . c0 . SE $ (map (c0 . SAtom . AChar)) $ C.unpack c
  where 
    t = C.pack . encodeCookieStr . T.unpack . prettyPrint
    
searchCookies k [] = Nothing
searchCookies k (x:xs) = case cookName x == k of
  True -> Just $ C.pack . decodeCookieStr . C.unpack $ cookValue x
  False -> searchCookies k xs
      
spb_set_memory ::
  [CSExp] 
  -> InterpM SPBM CSExp
spb_set_memory ((c, SE s):v:[]) = do
  let k = map extractChar s
  s <- liftBase S.get
  let env = spbEnv s
  liftIO . atomically $ setInMemory (T.pack k) (snd v) env
  memState <- liftIO . atomically $ readTVar (memoryDB env)
  liftIO $ putStrLn $ "Memory state set: " ++ k
    ++ " : " ++ show (snd v) ++ ";; " 
  liftIO $ putStrLn $ show memState
  return $ c0 $ SE []

      
spb_get_memory ::
  [CSExp] 
  -> InterpM SPBM CSExp
spb_get_memory ((c, SE s):[]) = do
  let k = map extractChar s
  s <- liftBase S.get
  let env = spbEnv s
  r <- liftIO . atomically $ readFromMemory (T.pack k) env
  memState <- liftIO . atomically $ readTVar (memoryDB env)
  liftIO $ putStrLn $ "Memory state get key: " ++ k 
  liftIO $ putStrLn $ "Memory state get: " ++ show memState
  liftIO $ putStrLn $ "Memory state get return: " ++ show r
  return $ c0 $ fromMaybe (SE []) r
  
spb_set_session ::
  [CSExp] 
  -> InterpM SPBM CSExp
spb_set_session ((c, SE s):v:[]) = do
  let k = map extractChar s
  s <- liftBase S.get
  let env = spbEnv s
  t <- liftIO $ getCurrentTime
  sessState <- liftIO $ atomically $ readTVar (sessions env)
  liftIO $ putStrLn $ "Session state: " ++ show sessState
  sessKey <- case searchCookies "spb-sessionKey" (spbCookies s) of
    (Just val) -> return $ C.pack . decodeCookieStr . C.unpack $ val
    Nothing -> do 
      k <- liftIO $ fmap (C.pack . show) $ atomically $ do
        let tv = sessCount . spbEnv $ s
        modifyTVar tv (+1)
        readTVar tv
      (liftBase . S.put) $ s { spbCookies = Cookie Nothing Nothing 
                                            Nothing "spb-sessionKey" k : spbCookies s }
      return k
  liftIO . atomically $ setInSession sessKey t (T.pack k) (snd v) env  
  return $ c0 $ SE []
  
decodeCookieStr = (fromMaybe "") . (decString True)
encodeCookieStr = (encString True (const False))

spb_get_session ::
  [CSExp] 
  -> InterpM SPBM CSExp
spb_get_session ((c, SE s):[]) = do
  let k = map extractChar s
  s <- liftBase S.get
  let env = spbEnv s
  case searchCookies "spb-sessionKey" (spbCookies s) of
    (Just val) -> do
      liftIO . atomically $ do
        v <- readFromSession (C.pack . decodeCookieStr . C.unpack $ val) (T.pack k) env
        case v of
          Nothing -> return $ c0 $ SE []
          (Just v) -> return $ c0 v
    Nothing -> return $ c0 $ SE []