module AI where

import Board
import Debug.Trace

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

-- |weak move gen function:
-- |generates all moves next to a move of player's own color,
-- |or a couple moves in the middle if player's first turn
-- |(returning only one might lead to no moves being generated for the second player)
-- |note: this will generate lots of moves on occupied spaces or off the edge of the board,
-- |but buildTree will screen them out.
gen :: Board->Col->[Position]
gen b c = case pieces b of
  [] -> trace (show [(4,4),(5,5),(6,6)]) [(4,4),(5,5),(6,6)]
  [x]-> trace (show [(4,4),(5,5),(6,6)]) [(4,4),(5,5),(6,6)]
  xs -> trace (show (firstTwo $ surrounding xs c)) (take 10 (surrounding xs c))
  --[] -> [(4,4),(5,5),(6,6)]
  --_ -> take 10 $ surrounding (pieces b) c
  where firstTwo (one:two:xs) = [one, two]
        firstTwo a = a


-- |Takes a list of positions and their colors plus a color argument,
-- |and returns the board positions adjacent to a all the pieces of the given color. 
surrounding :: [(Position,Col)]->Col->[Position]
surrounding [] _ = []
surrounding (x:xs) Black = case snd x of
  Black->[(fst(fst x)-1,snd(fst x)-1),(fst(fst x)-1,snd(fst x)),(fst(fst x)-1,snd(fst x)+1),
      (fst(fst x),snd(fst x)-1),(fst(fst x),snd(fst x)+1),
      (fst(fst x)+1,snd(fst x)-1),(fst(fst x)+1,snd(fst x)),(fst(fst x)+1,snd(fst x)+1)]++surrounding xs Black
  White->surrounding xs Black
surrounding (x:xs) White = case snd x of
  Black->surrounding xs White
  White->[(fst(fst x)-1,snd(fst x)-1),(fst(fst x)-1,snd(fst x)),(fst(fst x)-1,snd(fst x)+1),
      (fst(fst x),snd(fst x)-1),(fst(fst x),snd(fst x)+1),
      (fst(fst x)+1,snd(fst x)-1),(fst(fst x)+1,snd(fst x)),(fst(fst x)+1,snd(fst x)+1)]++surrounding xs White

-- |Given a function to generate plausible moves (i.e. board positions)
-- |for a player (Col) on a particular board, generate a (potentially)
-- |infinite game tree.
-- 
-- |(It's not actually infinite since the board is finite, but it's sufficiently
-- |big that you might as well consider it infinite!)
--
-- |An important part of the AI is the 'gen' function you pass in here.
-- |Rather than generating every possible move (which would result in an
-- |unmanageably large game tree!) it could, for example, generate moves
-- |according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from 
                             -- here for opposite player

-- |Get the best next move from a (possibly infinite) game tree. This should
-- |traverse the game tree up to a certain depth, and pick the move which
-- |leads to the position with the best score for the player whose turn it
-- |is at the top of the game tree.
-- |if not at the max depth, iterate over all the gameTrees in next_moves
-- |if at the max depth, return the best position and value in nextMoves
-- data GameTree = GameTree { game_board :: Board,
  --                          game_turn :: Col,
    --                        next_moves :: [(Position, GameTree)] }

getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
--getBestMove _ tree = fst((next_moves tree)!!0)
--trace(show (displayPoss(next_moves tree)))
getBestMove maxD tree = getBestMoveRecursive (next_moves tree) maxD (8,8) 0

-- |Handles the top-level part of move choice that directly compares 
-- |the predicted desirability of the futures of the immediate possible moves.
getBestMoveRecursive :: [(Position,GameTree)]
                          -> Int -- ^ max simulation depth
                          -> Position -- ^ best position so far
                          -> Int -- ^ best value so far
                          -> Position -- ^ final move to make in the real game
getBestMoveRecursive [] _ best _ = best
getBestMoveRecursive (x:xs) maxD bestPos bestVal = if newVal>bestVal
                                                      then getBestMoveRecursive xs maxD newPos newVal
                                                      else getBestMoveRecursive xs maxD bestPos bestVal
                              where newPos = fst nextTree
                                    newVal = snd nextTree
                                    nextTree = bestOfTrees (next_moves (snd x)) maxD 1 (fst x)

displayPoss []=[]
displayPoss (x:xs)=[fst x]++displayPoss xs

-- |where the game tree recursion bottoms out. Evaluates various eventual board states and determines the best case.
bestOfEvals :: [(Position, GameTree)] -- ^ positions and their results to examine
                 -> Col -- ^ color to maximize for
                 -> Int -- ^ best value so far (for recursion along list)
                 -> Position -- ^ best position so far (for recursion along list)
                 -> (Position,Int) -- ^ best position found and its value
bestOfEvals [] _ bestV bestP = (bestP,bestV) --when you get to the end, return the max
bestOfEvals (x:xs) c bestV bestP = if newV > bestV
                                    then bestOfEvals xs c newV (fst x) -- new best value and position
                                    else bestOfEvals xs c bestV bestP  -- keep old best value and position
                              where newV = evaluate (game_board (snd x)) (other(game_turn (snd x)))

-- |If at the maximum depth, evaluate the boards in all the gametrees and return the position associated with the best one
  -- |aka call bestOfEvals on the list and return that
-- |If not at the maximum depth, evaluate the boards in all the gametrees, take the best one, and call bestOfTrees on it at depth+1
  -- |aka call bestOfEvals on the list, find the (Pos,gametree) associated with the winning (pos,int), call bestoftrees again 
bestOfTrees :: [(Position,GameTree)]
                  -> Int -- ^ max depth
                  -> Int -- ^ current Depth
                  -> Position -- ^ suggested top-level move position
                  -> (Position,Int) -- ^ suggested top-level move position and its eventual value
bestOfTrees [] _ _ _ = ((8,8),0) --on empty list, return 0 as value so last value in next list up wins the comparison
bestOfTrees (x:xs) maxD curD topPos = if maxD == (trace (show curD) curD)
                                then trace(show("deep enough")) (topPos,finalVal)--(bestOfEvals (next_moves (snd x)) simTurn 0 (8,8))
                                else trace (show "deeper") (bestOfTrees (findPos (x:xs) bestPos) maxD (curD+1) topPos)
                              where finalVal = snd(evals)
                                    bestPos = fst evals
                                    evals = bestOfEvals (next_moves (snd x)) simTurn 0 (8,8)
                                    simTurn = if (odd curD)
                                                then (game_turn (snd x)) --on first and odd levels, optimize for the current player
                                                else other ((game_turn (snd x))) --on even levels, optmize for the opponent
--bestOfTrees (x:xs) maxD curD = if maxD==trace(show(curD))curD 
--                                then bestOfEvals (next_moves (snd x)) (game_turn (snd x)) 0 (8,8) 
--                                else better bestHead bestTail
--                            where bestHead = bestOfTrees (next_moves (snd x)) maxD (curD+1)
--                                  bestTail = bestOfTrees xs maxD curD -- continue the list instead of going down a level

-- |Takes a list of positions and game trees and a position and returns the next_moves of the game tree paired with the position
findPos :: [(Position,GameTree)]
                -> Position
                -> [(Position,GameTree)]
findPos [] _ = []
findPos (x:xs) p | p==(fst x) = next_moves (snd x)
                 | otherwise = findPos xs p

-- | A deprecated helper function for bestOfEvals
-- | Takes two pairs of positions and evaluation values and returns the one with the higher value. A helper function
better :: (Position,Int) -> (Position,Int) -> (Position,Int)
better left right = if snd left > snd right  then left else right

-- |Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> IO World -- ^ current world state
            -> IO (IO World)
updateWorld t world =
  do w <- world
     if isAITurn (turn w) (ais w)
       then case checkWon (board w) of
           Just c  -> return world
           Nothing -> case makeMove (board w) (turn w)
               (getBestMove 5 (buildTree gen (board w) (turn w))) of
               Just b  -> return $ return w { board = b, turn = other (turn w) }
               Nothing -> return world
       else return world

-- |Determine whether it's an AI's turn to play
isAITurn :: Col -> (Int, Int) -> Bool
isAITurn Black (0, _) = False
isAITurn Black (1, _) = True
isAITurn White (_, 0) = False
isAITurn White (_, 1) = True

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either 
 player has won and display a message if so.
-}
