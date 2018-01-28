module RegexEquality where

import DFA
import qualified NFA
import qualified Regex

equal :: String -> String -> Either String Bool
equal a b = do
    a' <- Regex.parse a
    b' <- Regex.parse b
    return $
        toDfa a' == toDfa b'

toDfa =
    NFA.toDFA . NFA.fromRegex