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

data Coords = Coords { x::Int, y::Int } deriving Show

data Board = Board { board::[[Part]] }

instance Show Board where
      show (Board arr) = showBoardAll arr


-- ###################################################################
-- ##########     Init Board     #####################################
-- ###################################################################

initBoard size u = Board [ [ u | x <- [0..size-1] ] | y <- [0..size-1]]

initNewBoard size = initBoard size E


-- ###################################################################
-- ##########     Show Board     #####################################
-- ###################################################################

showLine:: [Part] -> String
showLine [] = ""
showLine (s:xs) =
    show s ++ " " ++ showLine xs     

showLineLn it xs 
    | it < 10 = " " ++ show it ++ ":  " ++ showLine xs ++ "\n"
    | it >= 10 = show it ++ ":  " ++ showLine xs ++ "\n"

showBoard it [] = ""
showBoard it (x:xs) = 
    showLineLn it x ++ showBoard (it+1) xs
    
showFirstLine it size
    | it < size = (alphabet !! it) ++ " " ++ showFirstLine (it+1) size
    | it >= size = ""
        
showBoardAll arr =
    "     " ++ showFirstLine 0 (length arr) ++ "\n" ++ showBoard 1 arr


-- ###################################################################
-- ##########     Insert In Board     ################################
-- ###################################################################

insertInRow arr y u = (element y .~ u) arr

insertInBoard (Board arr) x y u = Board $ (element x .~ insertInRow (arr !! x) y u) arr

insertBattle (Board arr) x y u 
	|((arr !! x) !! y) == E = insertInBoard (Board arr) x y u
	|((arr !! x) !! y) /= E = (Board arr)


-- ###################################################################
-- ##########     Is Won     #########################################
-- ###################################################################

won arr u = wonBoard (board arr) (board arr) u 0

wonBoard _ [] _ _ = False
wonBoard arr (x:xs) u it = ((wonLine arr it arr u 0) || (wonBoard arr xs u (it+1)))

wonLine _ _ [] _ _ = False
wonLine arr x (y:ys) u it = ((hasFive arr x it u) || (wonLine arr x ys u (it+1)))

hasFive arr x y u
	|((arr !! x) !! y) == u = ((hasFiveVertical arr x y u 5) || (hasFiveHorizontally arr x y u 5) || (hasFiveBias arr x y u 5))
	|((arr !! x) !! y) /= u = False

hasFiveVertical _ _ _ _ 0 = True
hasFiveVertical arr x y u it 
	|y >= 0 = ((((arr !! x) !! y) == u) && (hasFiveVertical arr x (y-1) u (it-1)))
	|y < 0 = False

hasFiveHorizontally _ _ _ _ 0 = True
hasFiveHorizontally arr x y u it 
	|x >= 0 = ((((arr !! x) !! y) == u) && (hasFiveHorizontally arr (x-1) y u (it-1)))
	|x < 0 = False

hasFiveBias _ _ _ _ 0 = True
hasFiveBias arr x y u it 
	|((x >= 0) && (y >= 0)) = ((((arr !! x) !! y) == u) && (hasFiveBias arr (x-1) (y-1) u (it-1)))
	|((x < 0) || (y < 0)) = False


-- ###################################################################
-- ##########     Tests Elements     #################################
-- ###################################################################

testBoard = initBoard 19 E
a = initBoard 19 E
b = insertInBoard a 3 3 X
c = insertInBoard b 3 4 X
d = insertInBoard c 3 5 X
e = insertInBoard d 3 6 X
f = insertInBoard e 3 7 X
g = insertInBoard f 3 8 X
h = insertInBoard f 3 5 O
