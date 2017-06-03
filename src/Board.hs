module Board where

import Data.String
import Control.Lens


alphabet = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","W","Y","Z"]

data Part = X | O | E 

instance Show Part where
    show X = "X"
    show O = "O"
    show E = "_"

data Coords = Coords { x::Int,
					   y::Int } deriving Show

data Board = Board { board::[[Part]] }

instance Show Board where
      show (Board arr) = showBoardAll arr

initBoard size u = Board [ [ u | x <- [0..size-1] ] | y <- [0..size-1]]

initNewBoard size = initBoard size E

showLine:: [Part] -> String
showLine [] = ""
showLine (s:xs) =
    show s ++ " " ++ showLine xs     

showLineLn it xs 
    | it < 10 = " " ++ show it ++ ":  " ++ showLine xs ++ "\n"
    | it >= 10 = show it ++ ":  " ++ showLine xs ++ "\n"
    
--showBoard:: Int -> [[Part]] -> String
showBoard it [] = ""
showBoard it (x:xs) = 
    showLineLn it x ++ showBoard (it+1) xs
    
showFirstLine it size
    | it < size = (alphabet !! it) ++ " " ++ showFirstLine (it+1) size
    | it >= size = ""
        
showBoardAll arr =
    "     " ++ showFirstLine 0 (length arr) ++ "\n" ++ showBoard 1 arr

--insertInBoard arr x y u = do
--    (((board arr) !! y) !! x) = u

insertInRow arr y u = (element y .~ u) arr
insertInBoard (Board arr) x y u = Board $ (element x .~ insertInRow (arr !! x) y u) arr

testBoard = initBoard 19 E


