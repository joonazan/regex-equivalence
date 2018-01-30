module Regex where

import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Text
import Data.Text
import Control.Arrow

data Regex
    = Character Char
    | NTimes Regex
    | Maybe Regex
    | OneOrMore Regex
    | OneOf [Regex]
    | Consecutive [Regex]
    deriving (Show, Eq)

parse :: Text -> Either String Regex
parse =
    left show .
    Parsec.parse (regex <* eof) "not a regex"

ifMany _ [x] = x
ifMany f list = f list

regex =
    fmap (ifMany OneOf) $ sepBy1 consecutive (char '|')

consecutive =
    fmap (ifMany Consecutive) $ many1 $ term

term = do
    base <- literalCharacter <|> parens

    modifier <- optionMaybe $ oneOf modifiers
    return $
        case modifier of
            Just '+' -> OneOrMore base
            Just '*' -> NTimes base
            Just '?' -> Maybe base
            Nothing -> base

modifiers = "+*?"
specialCharacters = modifiers ++ "|()\\"

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