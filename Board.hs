module Board where
-- Ewa Stachów

alphabet = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','W','Y','Z']

data Part = X | O | E deriving Eq

instance Show Part where
    show X = "X"
    show O = "O"
    show E = "_"

data Board = Board { board::[[Part]] }

instance Show Board where
      show (Board arr) = showBoardAll arr

data Coords = Coords { xPos::Int, yPos::Int } deriving Eq

instance Show Coords where
    show (Coords a b) = " ("++show a++","++show b++") "

instance Ord Coords where
    compare (Coords a b) (Coords c d)
        |(b == d) = compare a c
        |otherwise = compare b d


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
    "\n     " ++ showFirstLine 0 (length arr) ++ "\n" ++ showBoard 1 arr

showFirstLine:: Int -> Int -> String    
showFirstLine it size
--    | it < size = show (it+1) ++ " " ++ showFirstLine (it+1) size
    | it < size = [(alphabet !! it)] ++ " " ++ showFirstLine (it+1) size
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
-- ##########     Not Part     #######################################
-- ###################################################################

notPart:: Part -> Part
notPart X = O
notPart O = X
notPart E = E


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

finished = [X,X,X,X,X]

finish:: Board -> Bool
finish board = ((won board X) || (won board O))

won:: Board -> Part -> Bool
won (Board arr) u = wonBoard arr arr u 0

wonBoard:: [[Part]] -> [[Part]] -> Part -> Int -> Bool
wonBoard _ [] _ _ = False
wonBoard arr (x:xs) u it = ((wonLine arr it arr u 0) || (wonBoard arr xs u (it+1)))

wonLine:: [[Part]] -> Int -> [[Part]] -> Part -> Int -> Bool
wonLine _ _ [] _ _ = False
wonLine arr x (y:ys) u it = ((hasX arr x it u 5 finished) || (wonLine arr x ys u (it+1)))

hasX:: [[Part]] -> Int -> Int -> Part -> Int -> [Part] -> Bool
hasX arr x y u howMany table
    |(getPartFromArr arr x y) == u = ((hasXVertical arr x y u howMany table) || 
        (hasXHorizontally arr x y u howMany table) || 
        (hasXBias arr x y u howMany table) || 
        (hasXBiasCross arr x y u howMany table))
    |otherwise = False

hasXVertical:: [[Part]] -> Int -> Int -> Part -> Int -> [Part] -> Bool
hasXVertical _ _ _ _ 0 _ = True
hasXVertical _ _ _ _ _ [] = True
hasXVertical arr x y u it (t:table)
    |(areCoordsArrValid arr x y) && (t == E) = (((getPartFromArr arr x y) == t) && (hasXVertical arr x (y-1) u (it-1) table))
    |(areCoordsArrValid arr x y) && (t /= E) = (((getPartFromArr arr x y) == u) && (hasXVertical arr x (y-1) u (it-1) table))
    |otherwise = False

hasXHorizontally:: [[Part]] -> Int -> Int -> Part -> Int -> [Part] -> Bool
hasXHorizontally _ _ _ _ 0 _ = True
hasXHorizontally _ _ _ _ _ [] = True
hasXHorizontally arr x y u it (t:table)
    |(areCoordsArrValid arr x y) && (t == E) = (((getPartFromArr arr x y) == t) && (hasXHorizontally arr (x-1) y u (it-1) table))
    |(areCoordsArrValid arr x y) && (t /= E) = (((getPartFromArr arr x y) == u) && (hasXHorizontally arr (x-1) y u (it-1) table))
    |otherwise = False

hasXBias:: [[Part]] -> Int -> Int -> Part -> Int -> [Part] -> Bool
hasXBias _ _ _ _ 0 _ = True
hasXBias _ _ _ _ _ [] = True
hasXBias arr x y u it (t:table)
    |(areCoordsArrValid arr x y) && (t == E) = (((getPartFromArr arr x y) == t) && (hasXVertical arr x (y-1) u (it-1) table))
    |(areCoordsArrValid arr x y) && (t /= E) = (((getPartFromArr arr x y) == u) && (hasXBias arr (x-1) (y-1) u (it-1) table))
    |otherwise = False

hasXBiasCross:: [[Part]] -> Int -> Int -> Part -> Int -> [Part] -> Bool
hasXBiasCross _ _ _ _ 0 _ = True
hasXBiasCross _ _ _ _ _ [] = True
hasXBiasCross arr x y u it (t:table)
    |(areCoordsArrValid arr x y) && (t == E) = (((getPartFromArr arr x y) == t) && (hasXVertical arr x (y-1) u (it-1) table))
    |(areCoordsArrValid arr x y) && (t /= E) = (((getPartFromArr arr x y) == u) && (hasXBiasCross arr (x-1) (y+1) u (it-1) table))
    |otherwise = False
    

-- ###################################################################
-- ##########     Whose was last move     ############################
-- ###################################################################

lastMovePart:: Board -> Part -> Part
lastMovePart board whoseFirst = notPart (whoseMove board whoseFirst)


-- ###################################################################
-- ##########     Whose move     #####################################
-- ###################################################################

whoseMove:: Board -> Part -> Part
whoseMove (Board arr) whoseFirst
    | (howMany arr X) == (howMany arr O) = whoseFirst
    | (howMany arr X) > (howMany arr O) = O
    | otherwise = X

howMany:: [[Part]] -> Part -> Int
howMany [] u = 0
howMany (x:xs) u = howManyLine x u + howMany xs u

howManyLine:: [Part] -> Part -> Int
howManyLine [] u = 0
howManyLine (x:xs) u = howManyCoords x u + howManyLine xs u

howManyCoords:: Part -> Part -> Int
howManyCoords el u
    | el == u = 1
    | otherwise = 0

notHisMove:: Board -> Part -> Part
notHisMove board whoseFirst
    | (whoseMove board whoseFirst) == X = O
    | otherwise = X


-- ###################################################################
-- ##########     Neighborhood     ###################################
-- ###################################################################

neighborhood:: Board -> [Coords]
neighborhood (Board arr) 
    | ((howMany arr O)+(howMany arr X)) > 0 = removeDuplicates (neighborhoodLine arr 0 arr)
    | otherwise = [Coords 0 0]

neighborhoodLine:: [[Part]] -> Int -> [[Part]] -> [Coords]
neighborhoodLine arr ity [] = []
neighborhoodLine arr ity (x:xs) = ((neighborhoodCoord arr ity 0 x) ++ (neighborhoodLine arr (ity+1) xs))

neighborhoodCoord:: [[Part]] -> Int -> Int -> [Part] -> [Coords]
neighborhoodCoord arr ity itx [] = []
neighborhoodCoord arr ity itx (x:xs) = ((neighborhoodSelectedCoord arr itx ity) ++ (neighborhoodCoord arr ity (itx+1) xs))

neighborhoodSelectedCoord:: [[Part]] -> Int -> Int -> [Coords]
neighborhoodSelectedCoord arr x y
    | (getPartFromArr arr x y) /= E = createNeighborhood arr x y
    | otherwise = []

createNeighborhood:: [[Part]] -> Int -> Int -> [Coords]
createNeighborhood arr x y = 
     (isRightNeighbor arr (x-1) (y-1)) ++ (isRightNeighbor arr x (y-1)) ++
        (isRightNeighbor arr (x+1) (y-1)) ++ (isRightNeighbor arr (x-1) y) ++
        (isRightNeighbor arr (x+1) y) ++ (isRightNeighbor arr (x-1) (y+1)) ++
        (isRightNeighbor arr x (y+1)) ++ (isRightNeighbor arr (x+1) (y+1))

isRightNeighbor:: [[Part]] -> Int -> Int -> [Coords]
isRightNeighbor arr x y 
    | ((areCoordsArrValid arr x y) && ((getPartFromArr arr x y) == E)) 
        = [(Coords x y)]
    | otherwise = []

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs