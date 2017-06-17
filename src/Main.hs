module Main where

import Data.String
import Board as Br
import Minmax as Mm
import System.IO(hFlush, stdout)

-- zaczyna człowiek X
-- wymiar planszy to 15
-- deep 3

main::IO()
main = game (initBoard 15 E) "x"

game:: Board -> String -> IO ()
game board "x" = do
  if (Br.finish board) then
    execute board "have won"
  else do
    print board
    putStr "gomoku> "
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
  game (nextStep board O X 3) "x"
execute board "x" = do
  putStrLn "Vertical: "
  x <- getLine
  putStrLn "Horizontal: "
  y <- getLine
  if(((read x :: Int) < 1) || ((read x :: Int) > (length (Br.board board))) || ((read y :: Int) < 1) || ((read y :: Int) > (length (Br.board board)))) 
    then do
      putStrLn " \n Wrong Coords, Try once again \n "
      execute board "x"
  else game (insertToBoard board ((read x :: Int)-1) ((read y :: Int)-1) X) "pc"
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
