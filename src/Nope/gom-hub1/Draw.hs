module Draw(drawWorld) where

import Graphics.Gloss
import Board

spacing = 25

-- |Given a world state, draws the board, pieces on it, buttons,
-- a turn marker and an indicator of the winner if ended.
drawWorld :: IO World -> IO Picture
drawWorld world =
  do w <- world
     board_image <- loadBMP "Assets/board.bmp"
     game_images <- sequence [loadBMP "Assets/empty_piece.bmp",
                              loadBMP "Assets/black_piece.bmp",
                              loadBMP "Assets/white_piece.bmp"]
     button_images <- sequence [loadBMP "Assets/newgame_button.bmp",
                              loadBMP "Assets/undo_button.bmp",
                              loadBMP "Assets/save_button.bmp",
                              loadBMP "Assets/load_button.bmp"]
     turnmarker_images <- sequence [loadBMP "Assets/black_turnmarker.bmp",
                              loadBMP "Assets/white_turnmarker.bmp"]
     winscreen_images <- sequence [loadBMP "Assets/black_winscreen.bmp",
                              loadBMP "Assets/white_winscreen.bmp"]
     return $ Pictures (
       -- Draw a background for the board
       board_image
       -- Translate the board to the centre
       : Translate (half w) (half w)
         (Pictures (drawBoard (board w) (0, 0) [] game_images))
       -- Draw the buttons
       : Pictures (drawButtons (buttons w) [] button_images)
       -- Draw the turn marker
       : Pictures (turnMarker w turnmarker_images)
       -- If the game is finished, print a winner
       : checkEnd w winscreen_images
       )
  -- Find half-size of board to centre it
  where half w = fromIntegral (- ((size (board w) - 1) * spacing)) / 2

-- |Draws a Picture element at given coordinates.
drawImage :: Position -> Picture -> Picture
drawImage (x, y) = Translate (fromIntegral (x * spacing))
                             (fromIntegral (y * spacing))

-- |Draws the board along with all pieces on it.
drawBoard :: Board -> Position -> [Picture] -> [Picture] -> [Picture]
drawBoard b (x, y) ps bmps
    -- On the first iteration draw the board image, then continue as for any normal space
    | x == 0 && y == 0 = drawPiece b (x, y) bmps : drawBoard b (x + 1, y) ps bmps
    -- If the last space has been reached, draw the piece then return everything
    | x == size b - 1 && y == size b - 1 = drawPiece b (x, y) bmps : ps
    -- When the last column of the current row is reached, draw the piece then move down one row and back to the first column
    | x == size b - 1 = drawPiece b (x, y) bmps : drawBoard b (0, y + 1) ps bmps
    -- For any other space on the board, draw the piece and move on
    | otherwise = drawPiece b (x, y) bmps : drawBoard b (x + 1, y) ps bmps

-- |Draws a piece (Black, White, empty) at a given position on the board
drawPiece :: Board -> Position -> [Picture] -> Picture
drawPiece b pos bmps = case getPieceAt (pieces b) pos of
                            -- If there is a piece in this space, draw it
                            Just piece -> drawChip pos (snd piece) bmps
                            -- Otherwise draw an empty space
                            Nothing -> drawImage pos (head bmps)

-- |Draws a Black or White chip at a given position on the board
drawChip :: Position -> Col -> [Picture] -> Picture
drawChip (x, y) Black bmps = drawImage (x, y) (bmps!!1)
drawChip (x, y) White bmps = drawImage (x, y) (bmps!!2)

-- |Check whether the game is finished and prints winner if so
checkEnd :: World -> [Picture] -> [Picture]
checkEnd w bmps = case checkWon (board w) of
                    Just c  -> [ printWinner c bmps ]
                    Nothing -> []

-- |Prints out the winner
printWinner :: Col -> [Picture] -> Picture
printWinner Black bmps = drawImage (0, 0) (head bmps)
printWinner White bmps = drawImage (0, 0) (bmps!!1)

-- |Draws all buttons in the passed array
drawButtons :: [Button] -> [Picture] -> [Picture] -> [Picture]
drawButtons [] ps [] = ps
drawButtons btns ps bmps = Translate btn_x btn_y (head bmps) : drawButtons (tail btns) ps (tail bmps)
            where b = head btns
                  btn_x = fromIntegral $ (x (topLeft b) - x (botRight b)) `div` 2 + x (botRight b)
                  btn_y = fromIntegral $ (y (topLeft b) - y (botRight b)) `div` 2 + y (botRight b)

-- |Calls the 'drawTurnMaker' function with the current player's turn
turnMarker :: World -> [Picture] -> [Picture]
turnMarker world bmps = case turn world of
                            c -> [ drawTurnMarker c bmps ]

-- |Draws an indicator of who's turn it currently is
drawTurnMarker :: Col -> [Picture] -> Picture
drawTurnMarker Black bmps = drawImage (11, 8) (head bmps)
drawTurnMarker White bmps = drawImage (11, 8) (bmps!!1)
