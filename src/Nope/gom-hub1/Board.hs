module Board where

main::IO()
main = undefined

data Col = Black | White
  deriving (Show, Read, Eq)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

-- |A Board is a record containing the board size (a board is a square grid,
-- |n * n), the number of pieces in a row required to win, and a list 
-- |of pairs of position and the colour at that position.  So a 10x10 board 
-- |for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- |would be represented as:
--
-- |Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show

-- Board takes size, count to win and pieces array
initBoard :: Int -> Int -> [(Position, Col)] -> Board
initBoard = Board

-- |A function that returns to the last turn of the current player
-- |(so it actually undoes 2 turns)
undo :: IO World -> IO World
undo w = do world <- w
            return world { board = (board world)
              { pieces = remove (pieces (board world)) 2 } }

-- |Remove n items from the front of a list
remove :: [a] -> Int -> [a]
remove [] _ = []
remove xs 0 = xs
remove (x:xs) n = remove xs (n - 1)

-- |A Button contains it's top-left and bottom-right coordinates,
-- |the value that it displays and a function that changes the
-- |world state.
data Button = Button { topLeft :: Position,
                       botRight :: Position,
                       value :: String,
                       action :: IO World -> IO World }

-- |A Button that initiates a new world (= new game) when clicked
newGame :: Button
newGame = Button { topLeft = (-320, 235),
                   botRight = (-240, 215),
                   value = "New Game",
                   action = newWorld }
        where newWorld w = do world <- w
                              initWorld (size (board world))
                                (target (board world)) [] Black (ais world)

-- |A Button that rolls back one turn for the current player
undoButton :: Button
undoButton = Button { topLeft = (-320, 205),
                      botRight = (-240, 175),
                      value = "Undo Move",
                      action = undo }

--The name of the save file
savefile = "save.dat"

-- |A button to save the game
saveButton :: Button
saveButton = Button { topLeft = (-320, 170),
                      botRight = (-240, 140),
                      value = "Save Game",
                      action = save }

-- |Save the current game state to a file
save :: IO World -> IO World
save w = do world <- w
            --Overwrite the old save file and write the size and target of the current board
            writeFile savefile (show (size (board world)) ++ " " ++ show (target (board world)))
            --Write the position and colour of every space on the board
            forM_ (pieces (board world)) outputPiece
            return world

outputPiece :: (Position, Col) -> IO ()
outputPiece (p, c) = appendFile savefile ("\n" ++ show (fst p) ++ " " ++ show (snd p) ++ " " ++ show c)

-- |A button to load the save file
loadButton :: Button
loadButton = Button { topLeft = (-320, 135),
                      botRight = (-240, 105),
                      value = "Load Game",
                      action = load }

-- |Load the current game state from a file
load :: IO World -> IO World
load w = do world <- w
            initWorld new_size new_target new_ps new_turn (ais world)
         where f = readFile savefile -- Read in the raw save file data
               ls = splitOn "\n" (unsafePerformIO f) -- Split the save file into lines
               top = splitOn " " (head ls) -- Grab each word from the top line of the save file
               new_size = read (head top) :: Int -- First word of the top line is the saved game's board size
               new_target = read (top!!1) :: Int -- Second word of the top line is the saved game's target
               new_ps = parseSaveFile (tail ls) -- Parse the rest of the save file into a list of pieces
               new_turn = other (read (splitOn " " (last ls)!!2) :: Col) -- The third word in the final line is the colour of the player who went last (next turn is other colour)

-- |Parse the saved file to a list of pieces
parseSaveFile :: [String] -> [(Position, Col)]
parseSaveFile [] = []
parseSaveFile ls = (pos, col) : parseSaveFile (tail ls)
            where l = splitOn " " (head ls)
                  x = read (head l) :: Int
                  y = read (l!!1) :: Int
                  pos = (x, y)
                  col = read (l!!2) :: Col

allButtons :: [Button]
allButtons = [newGame, undoButton, saveButton, loadButton]

-- |Overall state is the board and whose turn it is, plus any further
-- |information about the world (this may later include, for example, player
-- |names, timers, information about rule variants, etc)
data World = World { board :: Board,
                     turn :: Col,
                     buttons :: [Button],
                     ais :: (Int, Int) }

initWorld :: Int -> Int -> [(Position, Col)] -> Col -> (Int, Int) -> IO World
initWorld s c ps t ais = return $ World (initBoard s c ps) t allButtons ais

-- |Semantinc function replacement
x = fst
-- |Semantinc function replacement
y = snd

-- |Get the current piece at the given coordinates, or Nothing
getPieceAt :: [(Position, Col)] -> Position -> Maybe (Position, Col)
getPieceAt [] pos = Nothing
-- Loop through the pieces and return the matching one if found
getPieceAt (p:ps) pos | fst p == pos = Just p
                      | otherwise = getPieceAt ps pos

-- |Play a move on the board; return 'Nothing' if the move is invalid
-- |(e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove b c pos = case getPieceAt (pieces b) pos of
  Just n  -> Nothing -- Position already occupied
  Nothing -> if x pos < 0 || x pos > size b - 1 ||
                y pos < 0 || y pos > size b - 1
              then Nothing -- Position off the board
              -- Insert the new piece addition
              else Just b { pieces = (pos, c) : pieces b }

-- |Check whether the board is in a winning state for either player.
-- |Returns 'Nothing' if neither player has won yet
-- |Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon b = checkPieces (pieces b) b

checkPieces :: [(Position, Col)] -> Board -> Maybe Col
checkPieces [] _ = Nothing
-- Count pieces on all 8 axes
checkPieces (p:ps) b | countPieces p pcs (0, 1) s col == t = Just col   -- N
                     | countPieces p pcs (0, -1) s col == t = Just col  -- S
                     | countPieces p pcs (1, 0) s col == t = Just col   -- E
                     | countPieces p pcs (-1, 0) s col == t = Just col  -- W
                     | countPieces p pcs (1, 1) s col == t = Just col   -- NE
                     | countPieces p pcs (-1, 1) s col == t = Just col  -- NW
                     | countPieces p pcs (1, -1) s col == t = Just col  -- SE
                     | countPieces p pcs (-1, -1) s col == t = Just col -- SW
                     | otherwise = checkPieces ps b
                     where col = snd p
                           pcs = pieces b
                           t = target b
                           s = size b

-- |Takes a piece, direction, size and colour, and counts pieces in that direction
countPieces :: (Position, Col) -> [(Position, Col)] -> (Int, Int) -> Int -> Col -> Int
-- Stop counting if the piece is of the opposite colour
countPieces (_, Black) _ _ _ White = 0
countPieces (_, White) _ _ _ Black = 0
-- Do not go out of bounds of the board
countPieces p ps d s c | x pos < 0 || x pos >= s || y pos < 0 || y pos >= s = 0
                       -- Or add up the following pieces
                       | otherwise =
                         -- Making sure a following piece exists
                         case getPieceAt ps (x pos + x d, y pos + y d) of
                             Just piece -> 1 + countPieces piece ps d s c
                             Nothing    -> 1
                       where pos = fst p

-- |Check if a given player has a given amount of pieces in a row
checkPartial :: Board -> Col -> Int -> Bool
checkPartial b col t = checkPieces (pieces b) (b { target = t }) == Just col

-- |An evaluation function for a minimax search. Given a board and a colour
-- |return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate b c | checkWon b == Just c = trace(show 1000) 1000
             | checkPartial b c 4 = trace(show 400) 400
             | checkPartial b c 3 = trace(show 300) 300
             | checkPartial b c 2 = trace(show 200) 200
             | otherwise = trace(show 100) 100
