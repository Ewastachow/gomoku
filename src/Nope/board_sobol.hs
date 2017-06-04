import Data.Map as Map
import Data.Maybe
import Data.Char
import Data.Tree
import Data.List

data Coordinates = Coordinates Int Int deriving (Eq, Show)

instance Ord Coordinates where
    compare (Coordinates a b) (Coordinates c d)
        |(b == d) = a `compare` c
        |otherwise = b `compare` d

data Color = White | Black deriving Eq

instance Show Color where
   show Black = "⚈"
   show White = "⚆"

data Board = Board (Map Coordinates Color)

instance Show Board where
    show = toString

emptyBoard = Board (Map.fromList [])

addToBoard (Board map) coordinates color = Board (Map.insert coordinates color map)

isEndOfGame (Board map) = 
    (hasWon map Black) || (hasWon map White)

hasWon map color = 
    checkEveryCoordinates [coords | coords <- Map.keys map, fromJust (Map.lookup coords map) == color]
    where
        checkEveryCoordinates listOfCoordinates = Prelude.or (Prelude.map (\coords -> (((countInRow coords listOfCoordinates) == 5) || ((countInCol coords listOfCoordinates) == 5) || 
            ((countInRightDiag coords listOfCoordinates) == 5)|| ((countInLeftDiag coords listOfCoordinates) == 5))) listOfCoordinates)
        countInRow (Coordinates x y) listOfCoordinates
            | ((Coordinates x y) `elem` listOfCoordinates) = 1 + countInRow (Coordinates (x+1) y) listOfCoordinates
            | otherwise = 0
        countInCol (Coordinates x y) listOfCoordinates
            | ((Coordinates x y) `elem` listOfCoordinates) = 1 + countInCol (Coordinates x (y+1)) listOfCoordinates
            | otherwise = 0
        countInRightDiag (Coordinates x y) listOfCoordinates
            | ((Coordinates x y) `elem` listOfCoordinates) = 1 + countInRightDiag (Coordinates (x+1) (y+1)) listOfCoordinates
            | otherwise = 0
        countInLeftDiag (Coordinates x y) listOfCoordinates
            | ((Coordinates x y) `elem` listOfCoordinates) = 1 + countInLeftDiag (Coordinates (x-1) (y+1)) listOfCoordinates
            | otherwise = 0

negateColor color
    | (color==White) = Black
    | otherwise = White

toString (Board map) =
    iterate([Coordinates x y | y <- [1..19], x <- [1..19]])
        where
            iterate [element] = (mapToCharacter element) ++ (newLine element)
            iterate (element:listOfCoordinates) = (mapToCharacter element) ++ (newLine element) ++ iterate(listOfCoordinates)
            mapToCharacter coordinates
                | (member coordinates map) = show(fromJust (Map.lookup coordinates map)) 
                | otherwise = " "
            newLine (Coordinates x y)
                | (x == 19) = "\n"
                | otherwise = ""

rateBoard (Board innerMap) color = 
    rate [x | x <- Map.keys innerMap, fromJust (Map.lookup x innerMap) == color] - rate [x | x <- Map.keys innerMap, fromJust (Map.lookup x innerMap) == negateColor color]
        where
            rate listOfCoordinates = sum (Prelude.map (\coords -> ((rateVerticalNeighbors coords listOfCoordinates) + (rateHorizontalNeighbors coords listOfCoordinates) + 
                (rateRightDiagonalNeighbors coords listOfCoordinates) + (rateLeftDiagonalNeighbors coords listOfCoordinates))) listOfCoordinates)
            rateVerticalNeighbors coords listOfCoordinates = rateNeighborsSequence coords listOfCoordinates 0 (-1) 0 1
            rateHorizontalNeighbors coords listOfCoordinates = rateNeighborsSequence coords listOfCoordinates (-1) 0 1 0
            rateRightDiagonalNeighbors coords listOfCoordinates = rateNeighborsSequence coords listOfCoordinates (-1) (-1) 1 1
            rateLeftDiagonalNeighbors coords listOfCoordinates = rateNeighborsSequence coords listOfCoordinates (-1) 1 1 (-1)
            rateNeighborsSequence (Coordinates x y) listOfCoordinates x_1 y_1 x_2 y_2
                | ((Coordinates (x+x_1) (y+y_1)) `elem` listOfCoordinates) || ((Coordinates (x+x_2) (y+y_2)) `elem` listOfCoordinates) = 1
                | otherwise = 0
            

hasNeighbors (Coordinates x y) listOfCoordinates = ((Coordinates (x-1) (y-1)) `elem` listOfCoordinates) || ((Coordinates x (y-1)) `elem` listOfCoordinates) ||
    ((Coordinates (x+1) (y-1)) `elem` listOfCoordinates) || ((Coordinates (x-1) y) `elem` listOfCoordinates) || 
    ((Coordinates (x+1) y) `elem` listOfCoordinates) || ((Coordinates (x-1) (y+1)) `elem` listOfCoordinates) ||
    ((Coordinates x (y+1)) `elem` listOfCoordinates) || ((Coordinates (x+1) (y+1)) `elem` listOfCoordinates)

possibleMoves (Board map) = [Coordinates x y | x<-[1..19], y<-[1..19], notMember (Coordinates x y) map, hasNeighbors (Coordinates x y) (Map.keys map)]

buildTree board color = Node board (Prelude.map (\board -> (buildTree board oppositeColor)) possibleBoards)
    where
        getListOfBoards _ _ [] = []
        getListOfBoards (Board b) color (x:xs) =[addToBoard (Board b) x color] ++ (getListOfBoards (Board b) color xs)
        oppositeColor = (negateColor color)
        possibleCoordinates = possibleMoves board
        possibleBoards = getListOfBoards board color possibleCoordinates

rateBoards [] _ = []
rateBoards ((Node currentNode _):listOfBoards) color = [rateBoard currentNode color] ++ rateBoards listOfBoards color

minmax (Node node children) color maxLevel = 
    exploreTree (Node node children) color 0 maxLevel 
    where
        exploreTree (Node node children) color level maxLevel
            | Prelude.null children = rateBoard node color
            | level>maxLevel = if (level `mod` 2 == 0) then maximum (rateBoards children color) else minimum (rateBoards children color)
            | level `mod` 2 == 0 = maximum (Prelude.map (\board -> (exploreTree board color (level+1) maxLevel)) children)
            | otherwise = minimum (Prelude.map (\board -> (exploreTree board color (level+1) maxLevel)) children)

boardFromMinmax board color = addToBoard board (moves!!fromJust(elemIndex (maximum minMaxOfMoves) minMaxOfMoves)) color
    where
        maxLevelOfMinmax = 5
        moves = possibleMoves board
        minMaxOfMoves = Prelude.map (\move -> (minmax (buildTree (addToBoard board move color ) color) color maxLevelOfMinmax)) moves