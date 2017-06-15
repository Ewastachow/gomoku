module Minmax where

import Data.String
import Control.Lens
import Data.List as List
import Data.Set as Set
import Board as Br


data Coords = Coords { xPos::Int, yPos::Int } deriving (Show, Eq)

data MinMaxTree = MinMaxTree { value::Int, board::Br.Board, nextSteps::[MinMaxTree]} | Empty

data ComparingTrees = ComparingTrees { mmTreevalue::Int, mmTree::MinMaxTree }

instance Ord Coords where
    compare (Coords a b) (Coords c d)
        |(b == d) = compare a c
        |otherwise = compare b d


-- ###################################################################
-- ##########     Neighborhood     ###################################
-- ###################################################################

neighborhood:: [[Part]] -> [Coords]
neighborhood arr 
    | (howMany arr (whoseMove (Br.Board arr))) > 0 = removeDuplicates (neighborhoodLine arr 0 arr)
    | (((howMany arr (whoseMove (Br.Board arr))) == 0) && ((length arr) > 5)) = ([Coords 4 4] ++ [Coords 3 4])
    | (((howMany arr (whoseMove (Br.Board arr))) == 0) && ((length arr) <= 5)) = ([Coords 0 0] ++ [Coords 1 1])

neighborhoodLine:: [[Part]] -> Int -> [[Part]] -> [Coords]
neighborhoodLine arr ity [] = []
neighborhoodLine arr ity (x:xs) = ((neighborhoodCoord arr ity 0 x) ++ (neighborhoodLine arr (ity+1) xs))

neighborhoodCoord:: [[Part]] -> Int -> Int -> [Part] -> [Coords]
neighborhoodCoord arr ity itx [] = []
neighborhoodCoord arr ity itx (x:xs) = ((neighborhoodSelectedCoord arr itx ity) ++ (neighborhoodCoord arr ity (itx+1) xs))

neighborhoodSelectedCoord:: [[Part]] -> Int -> Int -> [Coords]
neighborhoodSelectedCoord arr x y
    | ((arr !! x) !! y) /= (whoseMove (Br.Board arr)) = []
    | ((arr !! x) !! y) == (whoseMove (Br.Board arr)) = createNeighborhood arr x y

createNeighborhood:: [[Part]] -> Int -> Int -> [Coords]
createNeighborhood arr x y = 
     (isRightNeighbor arr (x-1) (y-1)) ++ (isRightNeighbor arr x (y-1)) ++
        (isRightNeighbor arr (x+1) (y-1)) ++ (isRightNeighbor arr (x-1) y) ++
        (isRightNeighbor arr (x+1) y) ++ (isRightNeighbor arr (x-1) (y+1)) ++
        (isRightNeighbor arr x (y+1)) ++ (isRightNeighbor arr (x+1) (y+1))

isRightNeighbor:: [[Part]] -> Int -> Int -> [Coords]
isRightNeighbor arr x y 
    | ((((arr !! x) !! y) == E) && (x >= 0) && (y >= 0) && (x < (length arr)) && (x < (length arr))) 
        = [(Coords x y)]
    | ((((arr !! x) !! y) /= E) || (x < 0) || (y < 0) || (x >= (length arr)) || (x >= (length arr))) 
        = []

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


-- ###################################################################
-- ##########     Rate Board     #####################################
-- ###################################################################

rateBoard board 
    | (won board X) == True = 1
    | (won board O) == True = -1
    | finish board == False = 0


-- ###################################################################
-- ##########     Whose move     #####################################
-- ###################################################################

whoseMove:: Board -> Part
whoseMove board
    | (howMany (Br.board board) X) == (howMany (Br.board board) O) = X
    | (howMany (Br.board board) X) > (howMany (Br.board board) O) = O
    | (howMany (Br.board board) X) < (howMany (Br.board board) O) = X

howMany [] u = 0
howMany (x:xs) u = howManyLine x u + howMany xs u

howManyLine [] u = 0
howManyLine (x:xs) u = howManyCoords x u + howManyLine xs u

howManyCoords el u
    | el == u = 1
    | el /= u = 0

notHisMove board 
    | (whoseMove board) == X = O
    | (whoseMove board) /= X = X


-- ###################################################################
-- ##########     MinMaxTree     #####################################
-- ###################################################################

generateMinMaxTree deep boardArr = generateMoves deep boardArr (whoseMove boardArr)

generateMoves:: Int -> Board -> Part -> MinMaxTree
generateMoves deep boardArr u
    | deep > 0 = MinMaxTree 0 boardArr (minMaxChildren deep boardArr (neighborhood (Br.board boardArr)) u)
    | deep == 0 = MinMaxTree 0 boardArr []

minMaxChildren:: Int -> Board -> [Coords] -> Part -> [MinMaxTree]
minMaxChildren deep boardArr [] u = []
minMaxChildren deep boardArr (x:xs) u = ((generateMove deep boardArr x u):[]) ++ (minMaxChildren deep boardArr xs u)

generateMove:: Int -> Board -> Coords -> Part -> MinMaxTree
generateMove deep boardArr (Coords x y) u = generateMinMaxTree (deep-1) (Br.insertToBoard boardArr (x+1) (y+1) u)
-- generateMove deep boardArr (Coords x y) u = generateMinMaxTree (deep-1) (Br.insertBattle boardArr (x+1) (y+1) u)


-- ###################################################################
-- ##########     Apply Values To MinMaxTree     #####################
-- ###################################################################

calculateMMTValue (MinMaxTree val boardArr []) = rateBoard boardArr
calculateMMTValue (MinMaxTree val boardArr children) = calculateMMTValueTab children

calculateMMTValueTab [] = 0
calculateMMTValueTab (x:xs) = (calculateMMTValue x) + calculateMMTValueTab xs

generateComparingTreeTable [] = []
generateComparingTreeTable (x:xs) = ((ComparingTrees (calculateMMTValue x) x):[]) ++ generateComparingTreeTable xs

-- ###################################################################
-- ##########     Select Best Board     ##############################
-- ###################################################################
    
selectBestFromComparingTreeTable:: [ComparingTrees] -> ComparingTrees -> ComparingTrees
selectBestFromComparingTreeTable [] bestOption = bestOption
selectBestFromComparingTreeTable (x:xs) bestOption
    | (mmTreevalue x) > (mmTreevalue (selectBestFromComparingTreeTable xs bestOption)) = x
    | (mmTreevalue x) <= (mmTreevalue (selectBestFromComparingTreeTable xs bestOption)) = (selectBestFromComparingTreeTable xs bestOption)

selectBestBoard:: MinMaxTree -> Br.Board
selectBestBoard minMaxTree = (Minmax.board (mmTree (selectBestFromComparingTreeTable (generateComparingTreeTable (nextSteps minMaxTree)) (ComparingTrees (-20) Minmax.Empty))))

nextStep:: Br.Board -> Br.Board
nextStep boardArr = selectBestBoard (generateMinMaxTree 3 boardArr)












-- ## ma zwrocic liste coords
-- moreNeighborhood board ratio =  

-- ## ma stworzyć drzewo o głębokości deep dla borda
-- createMinMaxTree board ratio deep

-- ## czyj ruch teraz?
-- whoseStep board = 

    -- wygenerować sasiedztwo, policzyc dla nich mozliwosci
    -- funkcja oceny planszy musi byc ambitniejsza
    -- np. przerobienie sprawdzania czy jest zwyciestwo
