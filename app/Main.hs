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
        case R.equal a b of
            Right True ->
                "Equivalent!"
            Right False ->
                "Not equivalent!"
            Left s ->
                "Error parsing: " ++ s