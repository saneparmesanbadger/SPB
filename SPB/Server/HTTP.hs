{-# LANGUAGE OverloadedStrings #-}
module SPB.Server.HTTP (
  HTTPStatus (..)
  ,ContentType (..)
  ,Cookie (..)
  ,simpleCookie
  ,Request (..)
  ,UserInfo (..)
  ,parseRequest
  ,Response (..)  
  ,httpResponse
  ,httpResponseHeader) where
import Data.Monoid ( (<>))
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import System.CurrentLocale (currentLocale)
import Data.Time ( UTCTime)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.Format (formatTime, FormatTime)
import qualified Data.HashMap.Strict as HM
import Network.URL (decString)
import Debug.Trace (trace)
type ByteStringMap = HM.HashMap S.ByteString S.ByteString

data HTTPStatus = 
  S_200_OK 
  | S_204_No_Content
  | S_301_Moved S.ByteString 
  | S_302_Found
  | S_400_BadRequest 
  | S_403_Forbidden
  | S_404_NotFound                                             
  deriving (Show, Eq)           
           
data ContentType =                   
  Text_HTML
  | Text_Javascript
  | CotentType S.ByteString
  deriving (Show, Eq)           
    
data Cookie = Cookie {    
  cookDomain  :: Maybe S.ByteString
  ,cookPath   :: Maybe S.ByteString
  ,cookExpire :: Maybe UTCTime
  ,cookName   :: S.ByteString
  ,cookValue  :: S.ByteString
  } deriving (Show, Eq)           
              
simpleCookie :: 
  S.ByteString    -- ^Key
  -> S.ByteString -- ^Value
  -> Integer       -- ^Max Age
  -> IO Cookie     
simpleCookie k v t = do  
  t0 <- getCurrentTime
  return $ Cookie Nothing Nothing 
    (Just $ addUTCTime (fromInteger t) t0) k v
                  
status :: HTTPStatus -> (Int, S.ByteString)                  
status S_200_OK = (200, "OK")
status S_204_No_Content = (204, "No Content")
status (S_301_Moved s) = (301, "Moved Permanently")
status S_302_Found = (302, "Found")
status S_400_BadRequest = (400, "Bad Request")
status S_403_Forbidden = (403, "Forbidden")
status S_404_NotFound = (404, "Not Found")


-- http://hackage.haskell.org/packages/archive/time/1.4.0.2/doc/html/Data-Time-Format.html
-- example: Thu, 20 May 2004 21:12:58 GMT
httpResponseFormat = "%a, %d %b %Y %X %Z"
serverString = "SaneParmesanBadger/1.0"

httpResponseHeader :: HTTPStatus 
                      -> S.ByteString    -- ^Content Type
                      -> Maybe UTCTime   -- ^Last Modified
                      -> S.ByteString    -- ^Content
                      -> [Cookie]
                      -> IO S.ByteString 
httpResponseHeader s ctype tmodM content cookies = do 
  t  <- httpResponseTime =<< getCurrentTime
  tmod <- httpResponseTime =<< case tmodM of
    Nothing -> getCurrentTime
    (Just tm) -> return tm 
  locale <- currentLocale
  return $ 
    "HTTP/1.1 " <> (C.pack . show . fst $ status s) <> " " <> snd (status s)
    <> "\nDate: " <> (C.pack t)
    <> "\nConnection: Close"
    <> "\nServer: " <> serverString
    <> "\nAccept-Ranges: bytes"
    <> "\nContent-Type: " <> ctype
    <> C.concat (map (cookieString locale) cookies)
    <> "\nContent-Length: " <> (C.pack . show . S.length $ content)
    <> "\nLast-Modified: " <> (C.pack tmod)
    <> "\n\n"
        
cookieString locale c = 
  "\nSet-Cookie: " <> cookName c <> "=" <> cookValue c
  <> ";" <> expires <> path <> domain
  where
    cookieMaybe prefix may = 
      fromMaybe "" $ fmap (\x -> prefix <> ": " <> x <> ";") may
    expires = cookieMaybe "Expires" $ fmap (C.pack . (httpResponseTimeL locale)) $
              cookExpire c
    path = cookieMaybe "Path" $ cookPath c
    domain = cookieMaybe "Domain" $ cookDomain c

data Response = Response {
  responseStatus    :: HTTPStatus
  ,contentType      :: ContentType
  ,responseContents :: S.ByteString
  ,responseModified :: Maybe UTCTime
  ,responseCookies  :: [Cookie]
  } deriving (Show, Eq)           
  
httpResponse :: Response -> IO S.ByteString
httpResponse r = do
  header <- httpResponseHeader (responseStatus r) (contentType2Str $ contentType r) 
   (responseModified r) (responseContents r) (responseCookies r)
  return $ header <> responseContents r

httpResponseTime :: (FormatTime t) => t -> IO String
httpResponseTime t = (return . (\x -> httpResponseTimeL x t)) =<< currentLocale
  
httpResponseTimeL locale t = formatTime locale httpResponseFormat t  

data UserInfo = UserInfo {
  agent   :: S.ByteString
  ,ipAddr :: S.ByteString
  } deriving (Show, Eq)           
defUInfo = UserInfo "" ""                 

data RequestType = GET | POST | PUT 
                 deriving (Show, Eq)

data Request = Request {
  reqType    :: RequestType
  ,reqURL    :: S.ByteString
  ,reqHost   :: S.ByteString
  ,reqCType  :: ContentType
  ,reqCLen   :: Int
  ,getData   :: ByteStringMap
  ,postData  :: ByteStringMap
  ,userInfo  :: UserInfo
  ,cookies   :: HM.HashMap S.ByteString Cookie
  } deriving (Show, Eq)
                
defRq = Request GET "" "" Text_HTML 0 (HM.empty) (HM.empty) defUInfo HM.empty

str2ContentType :: S.ByteString -> ContentType
str2ContentType s = Text_HTML --TODO  

contentType2Str :: ContentType -> S.ByteString
contentType2Str c = "text/html" -- TODO

parseRequest :: S.ByteString -> Request
parseRequest str = 
  foldl parseRequestLine defRq $ C.lines str

parseRequestLine :: Request -> S.ByteString -> Request
parseRequestLine r s = 
  let words = C.words s 
  in case length words > 0 of
    False -> r
    True -> case head words of
      "GET" -> r { reqType = GET, reqURL = words !! 1 }
      "POST" -> r { reqType = POST, reqURL = words !! 1 }
      "Host:" -> r { reqHost = words !! 1 }
      "User-Agent:" -> r { userInfo = 
                             ((userInfo r) { agent = C.intercalate " " (tail words)})}
      "Content-Type:" -> r { reqCType = str2ContentType (words !! 1) }
      "Content-Length:" -> r { reqCLen = read (C.unpack $ words !! 1) }
      "Cookie:" -> r { cookies = HM.union (cookies r) newCookies }
        where newCookies = 
                HM.fromList $ reverse {- firefox will send updated cookie version first -} $
                map ((\l -> (l !!0, Cookie Nothing Nothing Nothing (l !! 0) (l !! 1)))) $
                 filter (\x -> length x > 1) $ 
                 map ((map (C.filter (\x -> x /= ' ' && x /= ';' && x /= '='))) .
                      (C.groupBy (\c1 c2 -> c2 /= '=' && c2 /= ' '))) $ 
                 C.groupBy (\c1 c2 -> c1 /= ';' && c2 /= ';') $ C.intercalate " " (tail words)
      z -> case filter (\x -> x /= "&") $ 
                C.groupBy (\c1 c2 -> c2 /= '&' && c1 /= '&') s of 
        [] -> r
        l -> case (reqType r) of  --Presume this a GET or a POST line
          GET -> r { getData = HM.union (getData r) assocList }
          POST -> r { postData = HM.union (postData r) assocList }
          where
            assocList = HM.fromList $ 
                        map ((\l' -> (C.pack $ fromMaybe "" $ decString True $ C.unpack $ l' !! 0
                                       , (fst . C.breakSubstring "Expires:")
                                        $ C.pack $ fromMaybe "" $ decString True $ C.unpack 
                                        $ l' !! 1))) $ 
                        filter (\x -> length x > 1) $
                        map ((filter (\x -> C.length x > 1)) . 
                             (C.groupBy (\c1 c2 -> c1 /= '=' && 
                                                   c2 /= '=' && 
                                                   c2 /= ' ' &&
                                                   c1 /= ' '))) l
