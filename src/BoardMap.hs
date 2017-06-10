module Board where

import Data.String
import Control.Lens
import Prelude hiding (lookup)
import Data.Map 
import Data.List

alphabet = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","W","Y","Z"]

data Part = X | O | E deriving Eq

instance Show Part where
    show X = "X"
    show O = "O"
    show E = "_"

data Coords = Coords { x::Int, y::Int } deriving (Show, Eq)

instance Ord Coords where
    compare (Coords a b) (Coords c d)
        |(b == d) = a `compare` c
        |otherwise = b `compare` d

data Board = Board { board::(Map Coords Part), size::Int }

instance Show Board where
      show (Board arr size) = showBoard (Board arr size)

showBoard (Board arr size) = intercalate "" [((showCell (getMap (Board arr size)) (Coords(x,y))) ++nextRow size y)| x <- [0..(size-1)], y <- [0..(size -1)]]

showCell m pos = " " ++ (show m) ++ " "

nextRow size y
    | y /= (size-1) = ""
    | y == (size-1) = "\n"

getMap (Board(m, _)) = m

toString (Board arr size) =
    iterate([Coordinates x y | y <- [0..(size-1)], x <- [0..(size-1)]])
        where
            iterate [element] = (mapToCharacter element) ++ (newLine element)
            iterate (element:listOfCoordinates) = (mapToCharacter element) ++ (newLine element) ++ iterate(listOfCoordinates)
            mapToCharacter coordinates
                | (member coordinates map) = show(fromJust (Map.lookup coordinates map)) 
                | otherwise = " "
            newLine (Coordinates x y)
                | (x == 19) = "\n"
                | otherwise = ""

--instance Show (Board) where
--    show x = showBoard x

--mapRows = 19
--cords = [1..mapRows]

--showBoard :: Board -> [Char]
--showBoard board = intercalate "" [((showCell (getMap board) (Pos(x, y)))++nextRow y)| x <-cords, y<-cords]

--showCell m pos
--    | GMap.lookup pos m == Nothing = " . "
--    | GMap.lookup pos m == Just B = " "++ (show B) ++" "
--    | GMap.lookup pos m == Just W = " "++ (show W) ++" "

--nextRow y
--    | y /= mapRows = ""
--    | y == mapRows = "\n"


--getMap (Board(m, _)) = m



