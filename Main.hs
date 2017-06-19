module Main where
-- Ewa Stachów

import Data.List
import Data.Maybe
import Board as Br
import Minmax as Mm
import System.IO(hFlush, stdout)


main::IO()
main = prepare

prepare:: IO()
prepare = do
    putStr "Board size (1 - 24): "
    hFlush stdout
    boardSize <- getLine
    step (initNewBoard (read boardSize::Int)) "x"    

step:: Board -> String -> IO()
step board "x" = do
    print board
    hFlush stdout
    gomoku board "x" (won board X) (won board O) (isBoardFull board)
step board "pc" = do
        print board
        gomoku board "pc" (won board X) (won board O) (isBoardFull board)

gomoku:: Board -> String -> Bool -> Bool -> Bool -> IO ()
gomoku _ "q" _ _ _ = do
    putStrLn "#####   Game Closed   #####"
    hFlush stdout
gomoku _ _ True _ _ = do
    putStrLn "#####   Player Won   #####"
    hFlush stdout
gomoku _ _ _ True _ = do
    putStrLn "#####   Computer Won   #####"
    hFlush stdout
gomoku _ _ _ _ True = do
    putStrLn "#####   Board Full   #####"
    hFlush stdout
gomoku board "pc" _ _ _ = do
    step (nextStep board O X 1) "x"
gomoku board "x" _ _ _= do
    putStr "Number: "
    hFlush stdout
    x <- getLine
    let xx = (read x::Int) 
    putStr "Letter: "
    hFlush stdout
    y <- getChar
    empty <- getLine
    if(y `elem` alphabet) then do
        let yy = ((fromJust $ elemIndex y alphabet)+1)
        if((xx < 1) || (xx > (length (Br.board board))) || (yy < 1) || (yy > (length (Br.board board)))) 
            then do
                putStrLn " \n #####   Wrong Coords, Try once again   ##### \n "
                hFlush stdout
                gomoku board "x" False False False
        else step (insertToBoard board (xx-1) (yy-1) X) "pc"
    else do
        putStrLn " \n #####   Wrong Coords, Try once again   ##### \n "
        hFlush stdout
        gomoku board "x" False False False
gomoku board _ _ _ _ = do
    putStrLn "#####   Something wrong   #####"
    hFlush stdout
    step board "x"