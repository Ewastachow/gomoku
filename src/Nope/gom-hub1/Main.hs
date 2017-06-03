module Main where

import Graphics.Gloss.Interface.IO.Game
import System.Environment

import Board
import Draw
import Input
import AI

-- |Get the arguments to start the game and call the 'start' function.

main :: IO ()
main = do args <- getArgs
          case args of
              -- User-specified size and count, and AI players
              [s, c, "0", "0"] -> start (read s :: Int) (read c :: Int) (0, 0)
              [s, c, "0", _]   -> start (read s :: Int) (read c :: Int) (0, 1)
              [s, c, _, "0"]   -> start (read s :: Int) (read c :: Int) (1, 0)
              [s, c, _, _]     -> start (read s :: Int) (read c :: Int) (1, 1)
              -- Or default to 19 and 5 with one AI
              _      -> start 19 5 (0, 0)

-- |Receive the game arguments and run the game.
--
-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated move
start :: Int -> Int -> (Int, Int) -> IO ()
start s c ais = playIO (InWindow "Gomoku" (640, 480) (10, 10))
                     (makeColor8 175 135 80 255) 10
                     (initWorld s c [] Black ais) -- in Board.hs
                     drawWorld -- in Draw.hs
                     handleInput -- in Input.hs
                     updateWorld -- in AI.hs
