{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.HTTP.Types.URI (queryToQueryText)
import Control.Monad (join)
import System.Environment (getEnv)
import Text.Hamlet
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Prelude hiding (null)
import Data.Text

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
        renderHtmlBuilder $ template result render
    result =
        handle $ queryToQueryText $ queryString req

data Route = Frontpage

render :: Route -> [(Text, Text)] -> Text
render Frontpage _ = "/"

template :: Maybe (Either Text Comparison) -> HtmlUrl Route
template result = [hamlet|
$doctype 5
<html>

    <head>
        <title>Regex Equivalence
        <meta name="viewport" content="width=device-width, initial-scale=1.0">

    <body>
        <h1>Regex Equivalence
        <p>Find out if two regular expressions match the same language

        <form action=@{Frontpage} method=GET>
            <input type=text name=a>
            <input type=text name=b>
            <input type=submit value=compare>

        $maybe res <- result
            $case res
                $of Left err
                    <p>#{err}
                $of Right Nothing
                    <p>They are equivalent!
                $of Right (Just (example, (matching, not_matching)))
                    <p>
                        '#{matching}' matches the 
                        $if null example
                            empty string 
                        $else
                            string '#{example}' 
                        unlike '#{not_matching}'
        $nothing
        
        <p>A subset of Perl-style regex is supported:
        <ul>
            <li> character -> character
            <li> x* -> zero or more x
            <li> x+ -> one or more x
            <li> x? -> one or zero x
            <li> foo|bar -> either foo or bar
            <li> parens to indicate precedence
            <li> backslash to escape aforementioned characters
|]

type Comparison = Maybe (Text, (Text, Text))

handle :: [(Text, Maybe Text)] -> Maybe (Either Text Comparison)
handle q = do
    a <- join $ lookup "a" q
    b <- join $ lookup "b" q
    return $
        case R.counterexample a b of
            Right (Just (example, firstMatches)) ->
                Right (Just (pack example, (if firstMatches then (a, b) else (b, a))))

            Right Nothing -> Right Nothing
            Left err -> Left $ pack err
