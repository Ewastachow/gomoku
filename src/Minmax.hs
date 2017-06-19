module Minmax where

import Board as Br


data MinMaxTree = MinMaxTree { board::Br.Board, nextSteps::[MinMaxTree]} | Empty

data ComparingTree = ComparingTree { boardCT::Board, valueCT::Int }


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


-- ###################################################################
-- ##########     Rate Board     #####################################
-- ###################################################################

rate41 = [E,X,X,X,X]
rate42 = [X,X,X,E,X]
rate43 = [X,X,E,X,X]

rate31 = [E,E,X,X,X]
rate32 = [E,X,X,X,E]
rate33 = [E,X,E,X,X]
rate34 = [E,X,X,E,X]

rate21 = [E,E,E,X,X]
rate22 = [E,E,X,X,E]

rateBoard:: Board -> Part -> Int
rateBoard board whoComp
    | (won board whoComp) == True = 100000
    | (won board (Br.notPart whoComp)) == True = (-100000)
    | otherwise = calculateBoardValue board whoComp 

calculateBoardValue:: Board -> Part -> Int
calculateBoardValue (Board arr) whoComp = (howManyRate arr arr whoComp 0)

howManyRate:: [[Part]] -> [[Part]] -> Part -> Int -> Int
howManyRate _ [] _ _  = 0
howManyRate arr (x:xs) u it = (howManyRateLine arr it x u 0) + (howManyRate arr xs u (it+1) ) 

howManyRateLine:: [[Part]] -> Int -> [Part] -> Part -> Int -> Int 
howManyRateLine _ _ [] _ _  = 0
howManyRateLine arr x (y:ys) u it
    | y == u = (howManyRateCoords arr x it u) + (howManyRateLine arr x ys u (it+1))
    | y == (notPart u) = ((0 - (howManyRateCoords arr x it (notPart u))) + (howManyRateLine arr x ys u (it+1)))
    | otherwise = 0 + (howManyRateLine arr x ys u (it+1))

howManyRateCoords:: [[Part]] -> Int -> Int -> Part -> Int
howManyRateCoords arr x y u = calculateRate arr x y u

calculateRate:: [[Part]] -> Int -> Int -> Part -> Int
calculateRate arr x y u = (isRateCoords arr x y u rate41 1)*700 +
                          (isRateCoords arr x y u (reverse rate41) 0)*700 +
                          (isRateCoords arr x y u rate42 1)*700 +
                          (isRateCoords arr x y u (reverse rate42) 0)*700 +
                          (isRateCoords arr x y u rate43 1)*700 +
                          (isRateCoords arr x y u rate31 2)*300 + 
                          (isRateCoords arr x y u (reverse rate31) 0)*300 +
                          (isRateCoords arr x y u rate32 1)*300 + 
                          (isRateCoords arr x y u rate33 2)*300 + 
                          (isRateCoords arr x y u (reverse rate33) 0)*300 +
                          (isRateCoords arr x y u rate34 2)*300 + 
                          (isRateCoords arr x y u (reverse rate34) 0)*300 +
                          (isRateCoords arr x y u rate21 1)*30 +
                          (isRateCoords arr x y u (reverse rate21) 0)*30 +
                          (isRateCoords arr x y u rate22 1)*30 +
                          (isRateCoords arr x y u (reverse rate22) 0)*30 

isRateCoords:: [[Part]] -> Int -> Int -> Part -> [Part] -> Int -> Int
isRateCoords arr x y u table ilEPocz = 
    setRateCoordsValue 
        (hasXVertical arr x (y+ilEPocz) u (length table) table) 
        (hasXHorizontally arr (x+ilEPocz) y u (length table) table) 
        (hasXBias arr (x+ilEPocz) (y+ilEPocz) u (length table) table) 
        (hasXBiasCross arr (x+ilEPocz) (y-ilEPocz) u (length table) table) 

setRateCoordsValue:: Bool -> Bool -> Bool -> Bool -> Int
setRateCoordsValue True True True True = 4
setRateCoordsValue True True True False = 3
setRateCoordsValue True True False True = 3
setRateCoordsValue True False True True = 3
setRateCoordsValue False True True True = 3
setRateCoordsValue True True False False = 2
setRateCoordsValue True False True False = 2
setRateCoordsValue False True True False = 2
setRateCoordsValue True False False True = 2
setRateCoordsValue False True False True = 2
setRateCoordsValue False False True True = 2
setRateCoordsValue True False False False = 1
setRateCoordsValue False True False False = 1
setRateCoordsValue False False True False = 1
setRateCoordsValue False False False True = 1
setRateCoordsValue False False False False = 0

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
selectBestBoard (first:ctList) = (boardCT (chooseCTBest ctList first))

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

tmpRate1 = initNewBoard 7
tmpRate2 = insertToBoard tmpRate1 3 3 X
tmpRate3 = insertToBoard tmpRate2 2 2 O
tmpRate4 = insertToBoard tmpRate3 3 4 X
tmpRate5 = insertToBoard tmpRate4 1 1 O
tmpRate6 = insertToBoard tmpRate5 3 5 X
tmpRate7 = insertToBoard tmpRate6 1 2 O
tmpRate8 = insertToBoard tmpRate7 3 2 X
tmpRate9 = insertToBoard tmpRate8 0 2 O
tmpRate10 = insertToBoard tmpRate9 3 1 X
tmpRate11 = insertToBoard tmpRate10 0 1 O
tmpRate12 = insertToBoard tmpRate11 5 6 X
testRate1 = rateBoard tmpRate7 O
testRate2 = rateBoard tmpRate8 O
testRate3 = rateBoard tmpRate9 O
testRate4 = rateBoard tmpRate10 O
testRate5 = rateBoard tmpRate11 O

tmp2Rate1 = initNewBoard 5
tmp2Rate2 = insertToBoard tmp2Rate1 1 1 X
tmp2Rate3 = insertToBoard tmp2Rate2 1 2 X
tmp2Rate4 = insertToBoard tmp2Rate3 1 3 X
tmp2Rate5 = insertToBoard tmp2Rate4 1 4 X
test2Rate1 = rateBoard tmp2Rate5 O