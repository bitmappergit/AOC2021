{-# LANGUAGE BlockArguments, LiberalTypeSynonyms #-}

import Text.Parsec
import Text.Parsec.String
import Lens 
import Data.Function

data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show, Eq)

constructor :: String -> (Int -> a) -> Parser a
constructor name constr = string name >> space >> fmap (constr . read) (many1 digit)

command :: Parser Command
command = choice [ constructor "forward" Forward
                 , constructor "down" Down
                 , constructor "up" Up
                 ]

file :: Parser [Command]
file = sepEndBy command spaces


data Sub
  = Sub { _horizontal :: Int
        , _vertical :: Int
        , _aim :: Int
        } deriving (Show, Eq)

horizontal, vertical, aim :: Mono Lens Sub Int
horizontal = lens _horizontal \o n -> o { _horizontal = n }
vertical = lens _vertical \o n -> o { _vertical = n }
aim = lens _aim \o n -> o { _aim = n }

run :: (Command -> Sub -> Sub) -> [Command] -> Sub -> Int
run stepper input sub =
  case input of
    x : xs -> run stepper xs (stepper x sub)
    [] -> sub % horizontal * sub % vertical


step1 :: Command -> Sub -> Sub
step1 command sub =
  case command of
    Forward n -> sub & horizontal +~ n
    Down n -> sub & vertical +~ n
    Up n -> sub & vertical -~ n

step2 :: Command -> Sub -> Sub
step2 command sub =
  case command of
    Forward n -> sub & horizontal +~ n & vertical +~ n * sub % aim
    Down n -> sub & aim +~ n
    Up n -> sub & aim -~ n

main :: IO () 
main = do
  Right input <- parseFromFile file "Day2.txt"
  print $ run step1 input (Sub 0 0 0)
  print $ run step2 input (Sub 0 0 0)
