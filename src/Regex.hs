module Regex where

import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.ByteString
import Data.ByteString
import Control.Arrow

data Regex
    = Character Char
    | NTimes Regex
    | OneOrMore Regex
    | OneOf [Regex]
    | Consecutive [Regex]
    deriving (Show, Eq)

parse :: ByteString -> Either String Regex
parse =
    left show .
    Parsec.parse regex "not a regex"

ifMany _ [x] = x
ifMany f list = f list

regex =
    fmap (ifMany OneOf) $ sepBy1 consecutive (char '|')

consecutive =
    fmap (ifMany Consecutive) $ many1 $ term

term = do
    base <- literalCharacter <|> parens

    modifier <- optionMaybe $ oneOf "+*"
    return $
        case modifier of
            Just '+' -> OneOrMore base
            Just '*' -> NTimes base
            Nothing -> base

specialCharacters = "|+*().\\"

literalCharacter =
    fmap Character $
        (noneOf specialCharacters <|> escapedCharacter)

escapedCharacter = do
    char '\\'
    oneOf specialCharacters

parens = do
    char '('
    r <- regex
    char ')'

    return r