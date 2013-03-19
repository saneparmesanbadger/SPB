{-# LANGUAGE OverloadedStrings #-}
module SPB.Server.Server where

import Prelude hiding (catch)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad (unless)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Exception (try, evaluate, throw, catch, SomeException, Exception)
import qualified Control.Monad.State.Strict as S
import Network.Socket hiding (recv)
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, sendAll)
import Data.Monoid ( (<>) )
import Safe (readDef, readMay)
import qualified Data.HashMap.Strict as HM

import SPB.Server.HTTP (HTTPStatus (..), Response (..), Request (..), ContentType (..) 
 , Cookie (..), UserInfo (..), httpResponseHeader
 , parseRequest, httpResponse)
import Text.ParserCombinators.Parsec (parse)
import SPB.Server.Env (Env (..), Program, newEnv, readFromMemory, setInMemory)
import SPB.Test (interpreterTest)
import SPB.Base (prettyPrint, CSExp (..))
import SPB.BaseLib (baseLibSymbolMap)
import SPB.Interpreter
import SPB.Parse
import SPB.ParserTest (parserTest)
import SPB.Javascript
import SPB.SPBWebLib
{- Resources
  http://blog.stephencleary.com/2009/05/using-socket-as-server-listening-socket.html
  http://www.tcpipguide.com/free/t_HTTPResponseMessageFormat.htm
-}

serveParmesan :: IO ()
serveParmesan = withSocketsDo $ do
  --parserTest
  --interpreterTest
  env <- newEnv
  let port = "3001"
  putStrLn $ "Serving Parmesan on port " ++ port
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  loopServe sock env serveCustomer
  sClose sock
             
loopServe :: Socket -> Env -> ((Socket, SockAddr) -> Env -> IO ()) -> IO ()
loopServe sock env action = do
  (conn, remoteAddr) <- accept sock
  forkIO $ do
    action (conn, remoteAddr) env
    putStrLn $ "Closing connection with " ++ show remoteAddr
    sClose conn
  loopServe sock env action

serveCustomer :: (Socket, SockAddr) -> Env -> IO ()
serveCustomer (conn, addr) env = do 
  putStrLn $ "Connection from " ++ show addr
  dat <- recv conn 4096 -- Does 4096 limit our input from browser or just chunk it?
  putStrLn $ C.unpack $ "\t Req Data: " <> dat
  let req0 = parseRequest dat
      req = req0 { userInfo = (userInfo req0) { ipAddr = C.pack (show addr) }}
  response <- spbInterp env req
  responseStr <- httpResponse response
  sendAll conn $ responseStr -- >> serveCustomer (conn, addr)
  
spbInterp :: Program 
spbInterp env req = do
  input <- readFile "spbMap.map"
  let hm = (readMay input) :: Maybe (M.Map String String)
  case hm of
    Nothing -> return $ Response S_404_NotFound Text_HTML "Map file corrupt." Nothing []
    (Just map) -> 
      case M.lookup (C.unpack $ reqURL req) map of
        Nothing ->  return $ Response S_404_NotFound Text_HTML "404 File not found." Nothing []
        (Just filename) -> do
          p <- parseFile env req filename
          return $ Response S_200_OK Text_HTML (C.pack $ T.unpack $ snd p) Nothing (fst p)

eHandler :: SomeException -> IO ([Cookie], T.Text)
eHandler se = return ([], T.pack $ show se)

parseFile :: Env -> Request -> String -> IO ([Cookie], T.Text)
parseFile env req file = do
  b <- doesFileExist file
  (flip catch) eHandler $ case b of
    False -> return ([], "File " <> T.pack file <> " does not exist.")
    True -> do
      input <- readFile file
      case parse parseExpr file ("(do " ++ input ++ ")") of 
        Left err -> return $ ([], T.pack (show err))
        Right val -> do
          i <- (\x -> S.runStateT x $ SPBState ((map snd) . HM.toList $ cookies req) env "")
               ((runInterpM [spbWebSymbolMap env req, jsSymbolMap
                            , baseLibSymbolMap] $ evalM val))
          case fst i of
            Left e -> return $ ([], T.intercalate ". " e)
            Right (csexp, symbolMap) -> return (spbCookies (snd i), spbOutput (snd i))
