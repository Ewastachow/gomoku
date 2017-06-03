module Main where

import Data.String

main::IO()
main = undefined


data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

data Part = X | O | E 

instance Show Part where
    show X = "X"
    show O = "O"
    show E = "_"

--instance Read Part where
--    read "X" = X
--    read "O" = O
--    read "_" = E

--data Coords = Coords { x::Int
--                       y::Int} deriving Show                       
--data Point = Point { coords::Coords
--                     part::Part} deriving Show
--data Board = Board [Point] | Empty  deriving Show
--initBoard size = [Point (Coords x y) E | x <- [0..size-1], y <- [0..size-1]]
--testBoard = Board (initBoard 19)
-- [ ] !! nr -> zwraca element [nr]

data Board = Board { board::[[Part]] }

instance Show Board where
--    show (Board arr) = unlines [unwords [ show (arr !! x) | x <- [0..18]] y | y <- [0..18]]
--    show (Board arr) = unlines [unwords ( map show (arr !! y) )| y <- [0..18]]
--    show (Board arr) = unlines (map arr show)
      show (Board arr) = showBoard arr
        
--showBoard b = do
--    ((((arr !! y) | y <- [0..18]) !! x) | x <- [0..18])
--    // interlocate
-- wszystko ma byc rekurencja, ale jak to .. sciagnac 


initBoard size u =Board  [ [ u | x <- [0..size-1] ] | y <- [0..size-1]]

testBoard = initBoard 19 E

-- metoda readBoard - to ma dziwnie działać
-- to podobno zrobić splitem - tylko jak w haskellu? Oo
-- metoda wstawiajaca element do tablicy - nie robi sprawdzania i ruchu, robi sam ruch, oddzielna metoda robiaca sprawdzanie - isValid
-- i potem metoda wywołujaca te dwie wyzej
-- polsors - dziwna firma
-- 
-- 

{--String showBoard(int[] alpaka){
    String lama;
   lama = alpaka[0].toString;
    lama = lama+showBoard(alpaka.bezFirst());
    return lama;
--}

showLine:: [Part] -> String
showLine [] = ""
showLine (s:alpaka) =
    show s ++ showLine alpaka 


showLineLn xs = 
   showLine xs ++ "\n"
    
showBoard:: [[Part]] -> String
showBoard [] = ""
showBoard (x:xs) = 
    showLineLn x ++ showBoard xs
    
-- po read zrobić instancje read jak dla show
-- dla read trzeba 
-- visual codee
