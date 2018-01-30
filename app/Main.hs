{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Data.Binary.Builder
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Control.Monad
import System.Environment (getEnv)

import qualified RegexEquality as R
 
main = do
    port <- getEnv "PORT"
    run (read port) app

app :: Network.Wai.Request
    -> (Network.Wai.Response -> IO ResponseReceived)
    -> IO ResponseReceived
app req respond = respond $ responseBuilder status200 headers content where
    headers =
        [("Content-Type", "text/html")]
    content =
        static `mappend` dynamic
    static =
        fromByteString
        "<form action='/' method=GET>\
          \<input type=text name=a>\
          \<input type=text name=b>\
          \<input type=submit value=compare>\
        \</form>"
    dynamic =
        fromMaybe empty $ handle $ queryString req

handle :: [(ByteString, Maybe ByteString)] -> Maybe Builder
handle q = do
    a <- join $ lookup "a" q
    b <- join $ lookup "b" q
    return $ putStringUtf8 $
        case R.counterexample a b of
            Right Nothing ->
                "Equivalent!"
            Right (Just (example, firstMatches)) ->
                let
                    (a', b') =
                        if firstMatches then
                            (show a, show b)
                        else
                            (show b, show a)
                in
                    "Regex " ++ a' ++ " matches the string " ++ show example
                    ++ " unlike " ++ b' ++ "!"
            Left s ->
                "Error parsing: " ++ s