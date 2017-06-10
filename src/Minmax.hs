module Minmax where

import Data.String
import Control.Lens
import Data.List as List
import Data.Set as Set
import Board as Br


data Coords = Coords { xPos::Int, yPos::Int } deriving (Show, Eq)

data MinMaxTree = MinMaxTree { value::Int, board::Board, nextSteps::[MinMaxTree]} | 
                    MinMax { value::Int, board::Board }

instance Ord Coords where
    compare (Coords a b) (Coords c d)
        |(b == d) = compare a c
        |otherwise = compare b d


-- ###################################################################
-- ##########     Neighborhood     ###################################
-- ###################################################################

neighborhood arr = neighborhoodLine arr 0 arr

neighborhoodLine arr ity (x:xs) = Set.union (neighborhoodCoord arr ity 0 x) (neighborhoodLine arr (ity+1) xs)

neighborhoodCoord arr ity itx (x:xs) = Set.union (neighborhoodSelectedCoord arr itx ity) 
                                                (neighborhoodCoord arr ity (itx+1) xs)

neighborhoodSelectedCoord arr x y
    | ((arr !! x) !! y) == E  = Set.empty
    | ((arr !! x) !! y) /= E  = createNeighborhood arr x y

createNeighborhood arr x y = 
    Set.union 
    (Set.union (Set.union (isRightNeighbor arr (x-1) (y-1)) (isRightNeighbor arr x (y-1))) 
        (Set.union (isRightNeighbor arr (x+1) (y-1)) (isRightNeighbor arr (x-1) y))) 
    (Set.union (Set.union (isRightNeighbor arr (x+1) y) (isRightNeighbor arr (x-1) (y+1))) 
        (Set.union (isRightNeighbor arr x (y+1)) (isRightNeighbor arr (x+1) (y+1))))

isRightNeighbor arr x y 
    | ((((arr !! x) !! y) == E) && (x >= 0) && (y >= 0) && (x < (length arr)) && (x < (length arr))) 
        = Set.singleton (Coords x y)
    | ((((arr !! x) !! y) /= E) || (x < 0) || (y < 0) || (x >= (length arr)) || (x >= (length arr))) 
        = Set.empty


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

-- zakładam że zaczyna X
whoseMove:: Board -> Part
whoseMove board
    | (howMany (Br.board board) X) == (howMany (Br.board board) O) = X
    | (howMany (Br.board board) X) /= (howMany (Br.board board) O) = O

howMany (x:xs) u = howManyLine x u + howMany xs u

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

--nextMove:: Board -> Board
--nextMove boardArr = nextMoveDeep 3 boardArr

nextMoveDeep deep boardArr = generateMoves deep boardArr (whoseMove boardArr)

generateMoves:: Int -> Board -> Part -> MinMaxTree
generateMoves deep boardArr u = 
    MinMaxTree 0 boardArr (minMaxChildren deep boardArr (neighborhood (Br.board boardArr)) u)

minMaxChildren:: Int -> Board -> Set a -> Part -> [a]
minMaxChildren deep boardArr set u = ((generateMove deep boardArr (Set.elemAt 0 (Set.delete 0 set)) u):[]) ++ (minMaxChildren deep boardArr set u)

-- generateMove:: Int -> Board -> Coords -> MinMaxTree
generateMove deep boardArr (Coords x y) u
    | deep > 0 = nextMoveDeep (deep-1) (Br.insertInBoard x y u)
    | deep == 0 = MinMaxTree 0 (Br.insertInBoard x y u) []
    

-- minMax board 




-- ## ma zwrocic liste coords
-- moreNeighborhood board ratio =  

-- ## ma stworzyć drzewo o głębokości deep dla borda
-- createMinMaxTree board ratio deep

-- ## czyj ruch teraz?
-- whoseStep board = 

    -- wygenerować sasiedztwo, policzyc dla nich mozliwosci
    -- funkcja oceny planszy musi byc ambitniejsza
    -- np. przerobienie sprawdzania czy jest zwyciestwo