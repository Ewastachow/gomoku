module Minmax where

import Data.String
import Control.Lens
import Data.List as List
import Board as Br

data MinMaxTree = MinMaxTree { board::Br.Board, nextSteps::[MinMaxTree]} | Empty

data ComparingTrees = ComparingTrees { mmTreevalue::Int, mmTree::MinMaxTree }
data ComparingTree = ComparingTree { boardCT::Board, valueCT::Int }

-- Potrzebne jest nam przekazać 2 rzeczy, kto zaczyna i kim jest komputer

-- ###################################################################
-- ##########     MinMaxTree     #####################################
-- ###################################################################

generateMinMaxTree:: Int -> Board -> Part -> MinMaxTree
generateMinMaxTree deep boardArr whoStarted = generateMoves deep boardArr (whoseMove boardArr whoStarted) whoStarted

generateMoves:: Int -> Board -> Part -> Part -> MinMaxTree
generateMoves deep boardArr u whoStarted
    | deep > 0 = MinMaxTree boardArr (minMaxChildren deep boardArr (neighborhood boardArr) u whoStarted)
    | otherwise = MinMaxTree boardArr []

minMaxChildren:: Int -> Board -> [Coords] -> Part -> Part -> [MinMaxTree]
minMaxChildren deep boardArr [] u whoStarted = []
minMaxChildren deep boardArr (x:xs) u whoStarted = ((generateMove deep boardArr x u whoStarted):[]) ++ (minMaxChildren deep boardArr xs u whoStarted)

generateMove:: Int -> Board -> Coords -> Part -> Part -> MinMaxTree
generateMove deep boardArr (Coords x y) u whoStarted = generateMinMaxTree (deep-1) (Br.insertToBoard boardArr x y u) whoStarted
-- generateMove deep boardArr (Coords x y) u = generateMinMaxTree (deep-1) (Br.insertBattle boardArr (x+1) (y+1) u)


-- ###################################################################
-- ##########     Rate Board     #####################################
-- ###################################################################

rateBoard:: Board -> Part -> Int
rateBoard board whoComp
    | (won board whoComp) == True = 30
    | (won board (Br.notPart whoComp)) == True = -30
    | otherwise = 0


-- ###################################################################
-- ##########     Calculate Value     ################################
-- ###################################################################

minMaxTreeValue:: MinMaxTree -> Part -> Int
minMaxTreeValue (MinMaxTree boardArr []) whoComp = rateBoard boardArr whoComp
minMaxTreeValue (MinMaxTree boardArr nextSteps) whoComp = foldl (+) 0 [(minMaxTreeValue x whoComp) | x <- nextSteps]


-- ###################################################################
-- ##########     Create CT Table     ################################
-- ###################################################################


createComparingTreesTableZero:: Board -> Part -> [ComparingTree]
createComparingTreesTableZero board whoComp = [(ComparingTree (insertToBoard board (xPos x) (yPos x) whoComp) 0) | x <- (neighborhood board)]

setComparingTreeTableValue:: [ComparingTree] -> Part -> Part -> Int -> [ComparingTree]
setComparingTreeTableValue ctTable whoComp whoStarted deep = [(ComparingTree (boardCT x) (minMaxTreeValue (generateMinMaxTree deep (boardCT x) whoStarted) whoComp)) | x <- ctTable]


-- ###################################################################
-- ##########     Select Best Board     ##############################
-- ###################################################################

selectBestBoard:: [ComparingTree] -> Board
selectBestBoard ctList = (boardCT (chooseCTBest ctList (ComparingTree (initNewBoard 1) (-1000))))

chooseCTBest:: [ComparingTree] -> ComparingTree -> ComparingTree
chooseCTBest [] best = best
chooseCTBest (x:xs) (ComparingTree boardBest valueBest)
    | (valueBest >= (valueCT x)) = chooseCTBest xs (ComparingTree boardBest valueBest)
    | otherwise = chooseCTBest xs x


-- ###################################################################
-- ##########     Next Step     ######################################
-- ###################################################################

nextStep:: Board -> Part -> Part -> Int -> Board
nextStep board whoComp whoStarted deep = selectBestBoard (setComparingTreeTableValue (createComparingTreesTableZero board whoComp) whoComp whoStarted deep)

-- ###################################################################
-- ##########     Apply Values To MinMaxTree     #####################
-- ###################################################################

-- calculateMMTValue (MinMaxTree val boardArr []) = rateBoard boardArr
-- calculateMMTValue (MinMaxTree val boardArr children) = calculateMMTValueTab children

-- calculateMMTValueTab [] = 0
-- calculateMMTValueTab (x:xs) = (calculateMMTValue x) + calculateMMTValueTab xs

-- generateComparingTreeTable [] = []
-- generateComparingTreeTable (x:xs) = ((ComparingTrees (calculateMMTValue x) x):[]) ++ generateComparingTreeTable xs

-- ###################################################################
-- ##########     Select Best Board     ##############################
-- ###################################################################
    
-- selectBestFromComparingTreeTable:: [ComparingTrees] -> ComparingTrees -> ComparingTrees
-- selectBestFromComparingTreeTable [] bestOption = bestOption
-- selectBestFromComparingTreeTable (x:xs) bestOption
--     | (mmTreevalue x) > (mmTreevalue (selectBestFromComparingTreeTable xs bestOption)) = x
--     | (mmTreevalue x) <= (mmTreevalue (selectBestFromComparingTreeTable xs bestOption)) = (selectBestFromComparingTreeTable xs bestOption)

-- selectBestBoard:: MinMaxTree -> Br.Board
-- selectBestBoard minMaxTree = (Minmax.board (mmTree (selectBestFromComparingTreeTable (generateComparingTreeTable (nextSteps minMaxTree)) (ComparingTrees (-20) Minmax.Empty))))

-- nextStep:: Br.Board -> Br.Board
-- nextStep boardArr = selectBestBoard (generateMinMaxTree 3 boardArr)

-- ## ma zwrocic liste coords
-- moreNeighborhood board ratio =  

-- ## ma stworzyć drzewo o głębokości deep dla borda
-- createMinMaxTree board ratio deep

-- ## czyj ruch teraz?
-- whoseStep board = 

    -- wygenerować sasiedztwo, policzyc dla nich mozliwosci
    -- funkcja oceny planszy musi byc ambitniejsza
    -- np. przerobienie sprawdzania czy jest zwyciestwo



-- ###################################################################
-- ##########     TESTS     ##########################################
-- ###################################################################

testBoard = initNewBoard 5
testBoard1 = insertToBoard testBoard 1 2 X
testBoard2 = insertToBoard testBoard1 2 3 O
testBoard3 = insertToBoard testBoard2 1 4 X
testBoard4 = insertToBoard testBoard3 2 2 O
testBoard5 = insertToBoard testBoard4 1 3 X
testBoard6 = insertToBoard testBoard5 2 1 O
testBoard7 = insertToBoard testBoard6 1 2 X
testValueMMT = minMaxTreeValue (generateMinMaxTree 4 testBoard7 X) O


testSelectBest1 = selectBestBoard (setComparingTreeTableValue (createComparingTreesTableZero testBoard7 O) O X 3)