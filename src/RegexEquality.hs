module RegexEquality where

import Data.ByteString
import DFA
import qualified NFA
import qualified Regex

counterexample :: ByteString -> ByteString -> Either String (Maybe (String, Bool))
counterexample a b = do
    a' <- Regex.parse a
    b' <- Regex.parse b
    return $
        recognizedByOne (toDfa a') (toDfa b')

toDfa =
    NFA.toDFA . NFA.fromRegex