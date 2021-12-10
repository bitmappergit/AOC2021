import Data.List
import Data.String
import System.File

solve : Nat -> List Nat -> Nat
solve size input with (input)
  _ | [] = 0
  _ | _ :: xs = if sum (take size input) < sum (take size xs)
                  then solve size xs + 1
                  else solve size xs

main : HasIO io => io ()
main = do
  input <- readFile "Day1.txt"
  case input of
    Left err => print err
    Right str => do
      let nums = map cast $ lines str
      print $ solve 1 nums
      print $ solve 3 nums

