module Helpers where

import Text.Parsec
import Text.Parsec.String

integer :: Parser Int
integer = read <$> many1 digit

commaList :: Parser a -> Parser [a]
commaList a = a `sepBy1` char ','

search :: (i -> [a]) -> (i -> [b]) -> (a -> b -> c) -> i -> [[c]]
search f g h i = [[h a b | b <- g i] | a <- f i]
