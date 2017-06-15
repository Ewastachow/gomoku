module Main where

import Data.String
import Board as Br
import Minmax as Mm
import System.IO(hFlush, stdout)

main::IO()
main = game a "o"

game:: Board -> String -> IO ()
game board "o" = do
  if (Br.finish board) then
    execute board "have won"
  else do
    print board
    putStr "gomoku> "
    hFlush stdout
    execute board "o"
game board "pc" = do
  if (Br.finish board) then
    execute board "have won"
  else do
    print board
    execute board "pc"

execute :: Board -> String -> IO ()
execute _ "quit" = putStrLn "Game is closed."
execute board "pc" = do
  game (nextStep board) "o"
execute board "o" = do
  putStrLn "Vertical: "
  x <- getLine
  putStrLn "Horizontal: "
  y <- getLine
  game (insertBattle board (read x :: Int) (read y :: Int) O) "pc"
execute board "have won" =
  putStrLn (show board ++ "Player have won!")
execute board _ = do
  putStrLn "Commands that you can use: \n \
  \ o    = put O on board \n \
  \ x    = put X on board \n \
  \ quit = quit the game "
  game board "o"





testBoard = initBoard 19 E
a = initBoard 19 E
b = insertInBoard a 3 3 X
c = insertInBoard b 3 4 X
d = insertInBoard c 3 5 X
e = insertInBoard d 3 6 X
ee = insertInBoard d 0 0 O
eff = insertInBoard d 18 18 O
f = insertInBoard e 3 7 X
g = insertInBoard f 3 8 X
h = insertInBoard f 3 5 O
testSet = Mm.neighborhood (Br.board e)
    
-- po read zrobić instancje read jak dla show
-- dla read trzeba 
-- visual codee
