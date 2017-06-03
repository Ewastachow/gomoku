module Input(handleInput) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace

-- Spacing between points on the grid
spacing = 25

-- |Updates the world state given an input event.
handleInput :: Event -> IO World -> IO (IO World)
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world =
  do w <- world
    -- If the AI is playing, do not accept input
     if isAITurn (turn w) (ais w)
       then return $ handleButtons world (x, y)
       -- Handle player input
       else case checkWon (board w) of -- Check if the game has been finished
          -- Player won, check for button presses but not moves
          Just c  -> return $ handleButtons world (x, y)
          -- Try getting the position on the board of the point clicked
          Nothing -> case getPosition (0, 0) (floor x, floor y) (size (board w)) of
              -- If it is an actual position, make a move
              Just pos -> case makeMove (board w) (turn w) pos of
                  -- Set the new board and turn and check buttons for clicking
                  Just b  -> return $ handleButtons (nextTurn w b) (x, y)
                  -- Or make no change and check buttons for clicking
                  Nothing -> return $ handleButtons world (x, y)
              -- Otherwise there is nothing there, so ignore it
              -- and check the buttons for clicking
              Nothing  -> return $ handleButtons world (x, y)
  where nextTurn w b = return w { board = b, turn = other (turn w) }
        handleButtons w (x, y) = do world <- w
                                    checkButtons w (floor x, floor y) (buttons world)
handleInput e w = return w

-- |Determines if a mouse click was on a button and calls the button's action function if so.
checkButtons :: IO World -> Position -> [Button] -> IO World
checkButtons w _ [] = w
-- Where w is the world, xm;ym are mouse coordinates, b is button, bs is list
-- If the click was made on the button's position
checkButtons w (xm, ym) (b:bs) = if xm > x (topLeft b) &&
                                    xm < x (botRight b) &&
                                    ym < y (topLeft b) &&
                                    ym > y (botRight b) then
                                    -- Run the buttons's function and return
                                    action b w
                                    -- Otherwise check the rest of the buttons
                                    else checkButtons w (xm, ym) bs

-- |Finds where on the board the mouse was clicked. 
-- |Returns the grid position of the click, or nothing for an invalid click.
getPosition :: Position -> Position -> Int -> Maybe Position
-- Check all possible points on the grid and compare to converted coordinates
getPosition (xo, yo) (xm, ym) size -- (xo;yo coordinates of grid, xm;ym mouse coordinates)
  -- Last corner of grid, so check and return
  | xo == size && yo == size = isPos (xo, yo) (xm, ym) size
  -- End of row, so move up to next row
  | yo == size = case isPos (xo, yo) (xm, ym) size of
                     Just p  -> Just p
                     Nothing -> getPosition (xo + 1, 0) (xm, ym) size
  -- Just move along the row, incrementing the column index
  | otherwise = case isPos (xo, yo) (xm, ym) size of
                    Just p  -> Just p
                    Nothing -> getPosition (xo, yo + 1) (xm, ym) size

-- |Determines if a mouse click was on a playable position and thus is a valid move.
-- |Returns the grid position of the click, or nothing for an invalid click.
isPos :: Position -> Position -> Int -> Maybe Position
isPos (xo, yo) (xm, ym) size
  -- Check whether mouse is approximately at playable point
  | convert xo size >= xm - pad && convert xo size <= xm + pad &&
    convert yo size >= ym - pad && convert yo size <= ym + pad
    = Just (xo, yo)
  | otherwise = Nothing
  -- Convert board coordinates to pixel coordinates
  where pad = 10 -- padding (pixels) around each point
        convert a size = a * spacing - halfsize size
        halfsize size = ((size - 1) * spacing) `div` 2
