module Board where

import Data.String
import Data.List
import Control.Lens


alphabet = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","W","Y","Z"]

data Part = X | O | E deriving Eq

instance Show Part where
    show X = "X"
    show O = "O"
    show E = "_"

data Board = Board { board::[[Part]] }

instance Show Board where
      show (Board arr) = showBoardAll arr


-- ###################################################################
-- ##########     Init Board     #####################################
-- ###################################################################

initNewBoard:: Int -> Board
initNewBoard size = initBoard size E

initBoard:: Int -> Part -> Board
initBoard size u = Board [ [ u | x <- [0..size-1] ] | y <- [0..size-1]]


-- ###################################################################
-- ##########     Show Board     #####################################
-- ###################################################################

showBoardAll:: [[Part]] -> String        
showBoardAll arr =
    "     " ++ showFirstLine 0 (length arr) ++ "\n" ++ showBoard 1 arr

showFirstLine:: Int -> Int -> String    
showFirstLine it size
    | it < size = (alphabet !! it) ++ " " ++ showFirstLine (it+1) size
    | it >= size = ""

showBoard:: Int -> [[Part]] -> String
showBoard it [] = ""
showBoard it (x:xs) = 
    showLineLn it x ++ showBoard (it+1) xs

showLineLn:: Int -> [Part] -> String
showLineLn it xs 
    | it < 10 = " " ++ show it ++ ":  " ++ showLine xs ++ "\n"
    | it >= 10 = show it ++ ":  " ++ showLine xs ++ "\n"

showLine:: [Part] -> String
showLine [] = ""
showLine (s:xs) =
    show s ++ " " ++ showLine xs     


-- ###################################################################
-- ##########     Are Coords Valid     ###############################
-- ###################################################################

areCoordsBoardValid:: Board -> Int -> Int -> Bool
areCoordsBoardValid (Board arr) x y = areCoordsValid (length arr) x y

areCoordsArrValid:: [[Part]] -> Int -> Int -> Bool
areCoordsArrValid arr x y = areCoordsValid (length arr) x y

areCoordsValid:: Int -> Int -> Int -> Bool
areCoordsValid size x y = (isValidCoord size x) && (isValidCoord size y)

isValidCoord:: Int -> Int -> Bool
isValidCoord size x = (x >= 0) && (x < size)


-- ###################################################################
-- ##########     Is Occupied     ####################################
-- ###################################################################

isOccupiedBoard:: Board -> Int -> Int -> Bool
isOccupiedBoard (Board arr) x y = isOccupiedArr arr x y

isOccupiedArr:: [[Part]] -> Int -> Int -> Bool
isOccupiedArr arr x y = ((getPartFromArr arr x y) /= E)

-- ###################################################################
-- ##########     Get Part     #######################################
-- ###################################################################

getPartFromBoard:: Board -> Int -> Int -> Part
getPartFromBoard (Board boardArr) x y = getPartFromArr boardArr x y

getPartFromArr:: [[Part]] -> Int -> Int -> Part
getPartFromArr arr x y = ((arr !! x) !! y)


-- ###################################################################
-- ##########     Is Board Full     ##################################
-- ###################################################################

isBoardFull:: Board -> Bool
isBoardFull (Board arr) = isArrFull arr

isArrFull:: [[Part]] -> Bool
isArrFull [] = True
isArrFull (x:xs) = (isLineFull x) && (isArrFull xs)

isLineFull:: [Part] -> Bool
isLineFull [] = True
isLineFull (x:xs) = (x /= E) && (isLineFull xs)

-- ###################################################################
-- ##########     Insert In Board     ################################
-- ###################################################################

-- insertInRow arr y u = (element y .~ u) arr

-- insertInBoard (Board arr) x y u = Board $ (element x .~ insertInRow (arr !! x) y u) arr

-- insertInEmpty (Board arr) x y u 
-- 	|((arr !! x) !! y) == E = insertInBoard (Board arr) x y u
-- 	|((arr !! x) !! y) /= E = (Board arr)

-- insertBattle (Board arr) x y u
-- 	|((x >= 0 ) && (x < (length arr)) && (y >= 0 ) && (y < (length arr))) = insertInEmpty (Board arr) (x - 1) (y - 1) u
-- 	|((x < 0 ) || (x >= (length arr)) || (y < 0 ) || (y >= (length arr))) = (Board arr)

----------------------------------------------------------------------

insertToBoard:: Board -> Int -> Int -> Part -> Board
insertToBoard (Board arr) x y u 
	| (areCoordsValid (length arr) x y) = Board (insertToArr arr x y u)
	| otherwise = Board arr

insertToArr:: [[Part]] -> Int -> Int -> Part -> [[Part]]
insertToArr arr posX posY u = [[ (insertPart arr x y posX posY u) | y <- [0..(length arr)-1] ] | x <- [0..(length arr)-1]]

insertPart:: [[Part]] -> Int -> Int -> Int -> Int -> Part -> Part
insertPart arr x y posX posY u 
	| (x /= posX) || (y /= posY) = getPartFromArr arr x y
	| (x == posX) && (y == posY) && ((getPartFromArr arr x y) == E) = u
	| otherwise = getPartFromArr arr x y

-- ###################################################################
-- ##########     Is Won     #########################################
-- ###################################################################

finish:: Board -> Bool
finish board = ((won board X) || (won board O))

won:: Board -> Part -> Bool
won (Board arr) u = wonBoard arr arr u 0

wonBoard:: [[Part]] -> [[Part]] -> Part -> Int -> Bool
wonBoard _ [] _ _ = False
wonBoard arr (x:xs) u it = ((wonLine arr it arr u 0) || (wonBoard arr xs u (it+1)))

wonLine:: [[Part]] -> Int -> [[Part]] -> Part -> Int -> Bool
wonLine _ _ [] _ _ = False
wonLine arr x (y:ys) u it = ((hasFive arr x it u) || (wonLine arr x ys u (it+1)))

hasFive:: [[Part]] -> Int -> Int -> Part -> Bool
hasFive arr x y u
	|(getPartFromArr arr x y) == u = ((hasFiveVertical arr x y u 5) || (hasFiveHorizontally arr x y u 5) || (hasFiveBias arr x y u 5))
	|(getPartFromArr arr x y) /= u = False

hasFiveVertical:: [[Part]] -> Int -> Int -> Part -> Int -> Bool
hasFiveVertical _ _ _ _ 0 = True
hasFiveVertical arr x y u it 
	|y >= 0 = (((getPartFromArr arr x y) == u) && (hasFiveVertical arr x (y-1) u (it-1)))
	|y < 0 = False

hasFiveHorizontally:: [[Part]] -> Int -> Int -> Part -> Int -> Bool
hasFiveHorizontally _ _ _ _ 0 = True
hasFiveHorizontally arr x y u it 
	|x >= 0 = (((getPartFromArr arr x y) == u) && (hasFiveHorizontally arr (x-1) y u (it-1)))
	|x < 0 = False

hasFiveBias:: [[Part]] -> Int -> Int -> Part -> Int -> Bool
hasFiveBias _ _ _ _ 0 = True
hasFiveBias arr x y u it 
	|((x >= 0) && (y >= 0)) = (((getPartFromArr arr x y) == u) && (hasFiveBias arr (x-1) (y-1) u (it-1)))
	|((x < 0) || (y < 0)) = False

-- seecond bias


-- ###################################################################
-- ##########     Tests     ##########################################
-- ###################################################################

-- InitBoard

testInitE = initBoard 5 E
testInitX = initBoard 5 X
testInitO = initBoard 5 O
testInit = initNewBoard 6

-- Validity

testValidity1 = areCoordsBoardValid testInit 0 0 -- True
testValidity2 = areCoordsBoardValid testInit 5 5 -- True
testValidity3 = areCoordsBoardValid testInit 6 6 -- False
testValidity4 = areCoordsBoardValid testInit (-1) (-1) -- False

-- Occupied

testOccupied1 = isOccupiedBoard testInsert2 2 4 -- True
testOccupied2 = isOccupiedBoard testInsert2 1 5 -- False

-- Full

testFull1 = isBoardFull testInitX -- True
testFull2 = isBoardFull testInit -- False
testFull3 = isBoardFull (insertToBoard testInsert2 3 3 O) -- False

-- Insert

testInsert1 = insertToBoard testInit 4 4 X
testInsert2 = insertToBoard testInit 2 4 X
testInsert3 = insertToBoard testInit 3 1 X
testInsert4 = insertToBoard testInit 1 1 O
testInsert5 = insertToBoard testInit 0 0 O
testInsert6 = insertToBoard testInit 5 5 O
testInsert7 = insertToBoard testInit 6 6 O -- Still empty board

-- Get Part

testPart1 = getPartFromBoard testInsert3 3 1 -- X

-- Winning

