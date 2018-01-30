module RegexEquality where

import Data.Text
import DFA
import qualified NFA
import qualified Regex

counterexample :: Text -> Text -> Either String (Maybe (String, Bool))
counterexample a b = do
    a' <- Regex.parse a
    b' <- Regex.parse b
    return $
        recognizedByOne (toDfa a') (toDfa b')

toDfa =
    NFA.toDFA . NFA.fromRegex