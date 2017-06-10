module Board(
        Color,
        Board(..),
        Coordinates(..),
        emptyBoard,
        addToBoard,
        hasWon,
        negateColor,
        toString,
        hasNeighbors
    )where

import Data.Map as Map
import Data.Maybe
import Data.Char
import Data.Tree
import Data.List

data Coordinates = Coordinates Int Int deriving (Eq, Show)

instance Ord Coordinates where
    compare (Coordinates a b) (Coordinates c d)
        |(b == d) = compare a c
        |otherwise = compare b d

data Color = White | Black deriving Eq

instance Show Color where
   show Black = "X"
   show White = "O"

data Board = Board (Map Coordinates Color)

instance Show Board where
    show = toString

emptyBoard:: Board
emptyBoard = Board (Map.empty)

addToBoard:: Board->Coordinates->Color->Board
addToBoard (Board map) coordinates color = Board (Map.insert coordinates color map)

hasWon:: Board->Color->Bool
hasWon (Board map) color = Prelude.any checkAllDirections listOfCoordinates
    where
        listOfCoordinates = [coords | coords <- Map.keys map, fromJust (Map.lookup coords map) == color]
        isFiveInDirection howMany (Coordinates x y) (xTranslation, yTranslation)
            | (Coordinates x y) `elem` listOfCoordinates = True && (isFiveInDirection (howMany+1) (Coordinates (x+xTranslation) (y+yTranslation)) (xTranslation, yTranslation))
            | (Coordinates x y) `elem` listOfCoordinates && howMany==5 = True
            | otherwise = False
        checkAllDirections coords = Prelude.any (isFiveInDirection 1 coords) [(x,y)| x<-[-1..1], y<-[-1..1], x/=0 && y/=0]

negateColor:: Color->Color
negateColor color
    | (color==White) = Black
    | otherwise = White

joinListOfStrings:: [String]->String
joinListOfStrings [element] = element
joinListOfStrings (x:xs) = x ++ joinListOfStrings xs 

charToString:: Char->String
charToString a = [a]

toString:: Board->String
toString (Board map) =
    "  " ++ joinListOfStrings (Prelude.map charToString [x | x<-['A'..'S']]) ++ "\n" ++ joinListOfStrings (Prelude.map combine [Coordinates x y | x <- [1..19], y <- [1..19]])
        where
            combine s = index s ++ mapToCharacter s ++ newLine s
            mapToCharacter coordinates
                | (member coordinates map) = show(fromJust (Map.lookup coordinates map)) 
                | otherwise = " "
            newLine (Coordinates x y)
                | (y == 19) = "\n"
                | otherwise = ""
            index (Coordinates x y)
                | (y == 1 && x<10) = " " ++ show x
                | (y == 1 )  = show x
                | otherwise = ""
            
hasNeighbors:: Coordinates->[Coordinates]->Bool
hasNeighbors (Coordinates x y) listOfCoordinates = 
    Prelude.or (Prelude.map (\(xTranslation, yTranslation) -> ((Coordinates (x+xTranslation) (y+yTranslation)) `elem` listOfCoordinates)) [(x,y) | x<-[-1..1], y<-[-1..1], x/=0 && y/=0])
______________________________________________

module Game(
        rateBoard,
        rateNeighborsSequence,
        rateBoards,
        buildTree,
        minmaxAlfa,
        minmaxBeta,
        exploreTree,
        minmax,
        boardFromMinmax
    )where

import Data.Maybe
import Data.Tree
import Data.List
import Data.Map as Map
import Board

rateBoard:: Board->Color->Int
rateBoard (Board innerMap) color = 
    rate [x | x <- Map.keys innerMap, fromJust (Map.lookup x innerMap) == color] - rate [x | x <- Map.keys innerMap, fromJust (Map.lookup x innerMap) == negateColor color]
        where
            rate listOfCoordinates = sum (Prelude.map (rateAllDirections listOfCoordinates) listOfCoordinates)
            rateAllDirections listOfCoordinates coordinates = sum (Prelude.map (rateNeighborsSequence coordinates listOfCoordinates) [(x,y) | x<-[-1..0], y<-[-1..1], x/=0 && y/=(-1)])

rateNeighborsSequence:: Coordinates->[Coordinates]->(Int, Int)->Int
rateNeighborsSequence (Coordinates x y) listOfCoordinates (xTranslation, yTranslation)
    | ((Coordinates (x+xTranslation) (y+yTranslation)) `elem` listOfCoordinates) || ((Coordinates (x-xTranslation) (y-yTranslation)) `elem` listOfCoordinates) = 1
    | otherwise = 0

rateBoards:: [Tree Board]->Color->[Int]
rateBoards [] _ = []
rateBoards ((Node currentNode _):listOfBoards) color = [rateBoard currentNode color] ++ rateBoards listOfBoards color

possibleMoves:: Board->[Coordinates]
possibleMoves (Board map) 
    | (Map.null map) = [Coordinates x y | x<-[1..19], y<-[1..19]]
    | otherwise = [Coordinates x y | x<-[1..19], y<-[1..19], notMember (Coordinates x y) map, hasNeighbors (Coordinates x y) (Map.keys map)]

buildTree:: Board->Color->Tree Board
buildTree board color = Node board (Prelude.map (\board -> (buildTree board oppositeColor)) possibleBoards)
    where
        getListOfBoards _ _ [] = []
        getListOfBoards (Board b) color (x:xs) =[addToBoard (Board b) x color] ++ (getListOfBoards (Board b) color xs)
        oppositeColor = (negateColor color)
        possibleCoordinates = possibleMoves board
        possibleBoards = getListOfBoards board color possibleCoordinates

minmaxAlfa:: [Tree Board]->Color->Int->Int->Int->Int->Int
minmaxAlfa [] color level maxLevel alfa beta = alfa
minmaxAlfa (x:xs) color level maxLevel alfa beta 
    | (newAlfa>=beta) = beta 
    | otherwise  = minmaxAlfa xs color level maxLevel newAlfa beta
    where 
        newAlfa = max alfa (exploreTree x color level maxLevel alfa beta)

minmaxBeta:: [Tree Board]->Color->Int->Int->Int->Int->Int
minmaxBeta [] color level maxLevel alfa beta = beta
minmaxBeta (x:xs) color level maxLevel alfa beta
    | (alfa >= newBeta) = alfa
    | otherwise = minmaxBeta xs color level maxLevel alfa newBeta
    where 
        newBeta = min beta (exploreTree x color level maxLevel alfa beta)

exploreTree:: Tree Board->Color->Int->Int->Int->Int->Int
exploreTree (Node node children) color level maxLevel alfa beta
    | Prelude.null children = rateBoard node color
    | level==maxLevel = if (level `mod` 2 == 0) then maximum (rateBoards children color) else minimum (rateBoards children color)
    | level `mod` 2 == 0 = minmaxBeta children color (level+1) maxLevel alfa beta
    | otherwise = minmaxAlfa children color (level+1) maxLevel alfa beta

minmax:: Tree Board->Color->Int->Int
minmax (Node node children) color maxLevel = exploreTree (Node node children) color 0 maxLevel (-1000) 1000

boardFromMinmax:: Board->Color->Board
boardFromMinmax board color = addToBoard board (moves!!fromJust(elemIndex (maximum minMaxOfMoves) minMaxOfMoves)) color
    where
        maxLevelOfMinmax = 1
        moves = possibleMoves board
        minMaxOfMoves = Prelude.map (\move -> (minmax (buildTree (addToBoard board move color ) color) color maxLevelOfMinmax)) moves