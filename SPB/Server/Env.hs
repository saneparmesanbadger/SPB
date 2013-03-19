module SPB.Server.Env (
  Program
  , Env (..)
  , newEnv
  , readFromSession
  , setInSession
  , setInMemory    
  , readFromMemory ) where
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock (getCurrentTime, addUTCTime, UTCTime (..))
import Control.Concurrent.STM (STM (..))
import Control.Concurrent.STM.TVar (TVar (..), newTVarIO, readTVar, writeTVar, modifyTVar')
import SPB.Server.HTTP (HTTPStatus, ContentType, Request(..), Response (..))
import SPB.Base
type ByteStringMap = HM.HashMap BS.ByteString BS.ByteString
type SessionKey = BS.ByteString

type Program = Env -> Request -> IO Response

-- | Keep track of session data and keep an in-memory database
-- Uses STM to make everything thread-safe. 
data Env = Env {
  memoryDB :: TVar (HM.HashMap T.Text SExp)
  ,sessCount :: TVar Int
  ,sessions :: TVar (HM.HashMap SessionKey (UTCTime, HM.HashMap T.Text SExp))
  ,routeMap :: TVar (HM.HashMap BS.ByteString Program)
  }
           
newEnv :: IO Env           
newEnv = do
  mdb <- newTVarIO HM.empty
  sessC <- newTVarIO 0
  sess <- newTVarIO HM.empty
  rmap <- newTVarIO HM.empty
  return (Env mdb sessC sess rmap)


setInSession :: SessionKey 
                -> UTCTime
                -> T.Text -- ^Key 
                -> SExp -- ^Value
                -> Env
                -> STM ()
setInSession sessKey t k v env = do 
  sess <- readTVar $ sessions env
  writeTVar (sessions env) $
    case HM.lookup sessKey sess of
      Nothing -> 
        HM.insert sessKey (t, (HM.fromList [(k, v)])) sess
      (Just sessHM) ->
        HM.insert sessKey (t, (HM.insert k v $ snd sessHM)) sess
 
readFromSession :: SessionKey
                   -> T.Text -- ^Key
                   -> Env
                   -> STM (Maybe SExp)
readFromSession sessKey k env = do
  sess <- readTVar $ sessions env
  return $
    (HM.lookup k) =<< (fmap snd $ (HM.lookup sessKey sess))

setInMemory :: T.Text    -- ^Key
               -> SExp -- ^Value
               -> Env
               -> STM ()
setInMemory k v env =
  modifyTVar' (memoryDB env) $ \mem -> HM.insert k v mem

readFromMemory :: T.Text -- ^Key
                  -> Env
                  -> STM (Maybe SExp)
readFromMemory k env = do                   
  fmap (HM.lookup k) $ readTVar $ memoryDB env
