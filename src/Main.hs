module Main where

import Data.String
import Data.List
import Data.Maybe
import Board as Br
import Minmax as Mm
import System.IO(hFlush, stdout)

-- zaczyna człowiek X
-- wymiar planszy to 15
-- deep 3

-- fromJust $ elemIndex 3 [1,2,3,4]

main::IO()
main = game (initBoard 15 E) "x"

game:: Board -> String -> IO ()
game board "x" = do
  if (Br.finish board) then
    execute board "have won"
  else do
    print board
    hFlush stdout
    execute board "x"
game board "pc" = do
  if (Br.finish board) then
    execute board "have won"
  else do
    print board
    execute board "pc"

execute :: Board -> String -> IO ()
execute _ "quit" = putStrLn "Game is closed."
execute board "pc" = do
  game (nextStep board O X 2) "x"
execute board "x" = do
  putStr "Vertical: "
  x <- getLine
  let xx = (read x :: Int) 
  putStr "Horizontal: "
--  y <- getLine
--  let yy = (read y :: Int)
  y <- getChar
  let yy = ((fromJust $ elemIndex y alphabet)+1)
  if((xx < 1) || (xx > (length (Br.board board))) || (yy < 1) || (yy > (length (Br.board board)))) 
    then do
      putStrLn " \n Wrong Coords, Try once again \n "
      execute board "x"
  else game (insertToBoard board (xx-1) (yy-1) X) "pc"
execute board "have won" =
  putStrLn (show board ++ "Player have won!")
execute board _ = do
  putStrLn "Commands that you can use: \n \
  \ o    = put O on board \n \
  \ x    = put X on board \n \
  \ quit = quit the game "
  game board "x"





-- testBoard = initBoard 19 E
-- a = initBoard 19 E
-- b = insertInBoard a 3 3 X
-- c = insertInBoard b 3 4 X
-- d = insertInBoard c 3 5 X
-- e = insertInBoard d 3 6 X
-- ee = insertInBoard d 0 0 O
-- eff = insertInBoard d 18 18 O
-- f = insertInBoard e 3 7 X
-- g = insertInBoard f 3 8 X
-- h = insertInBoard f 3 5 O
-- testSet = Mm.neighborhood (Br.board e)
    
-- po read zrobić instancje read jak dla show
-- dla read trzeba 
-- visual codee
