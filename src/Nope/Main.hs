module Main where



main::IO()
main = undefined
data Part = X | O | E 

instance Show Part where
    show X = "X"
    show O = "O"
    show E = "_"

instance Read Part where
    read "X" = X
    read "O" = O
    read "_" = E

--data Coords = Coords { x::Int
--                       y::Int} deriving Show                       
--data Point = Point { coords::Coords
--                     part::Part} deriving Show
--data Board = Board [Point] | Empty  deriving Show
--initBoard size = [Point (Coords x y) E | x <- [0..size-1], y <- [0..size-1]]
--testBoard = Board (initBoard 19)
-- [ ] !! nr -> zwraca element [nr]

data Board = Board [[Part]] deriving Show
initBoard size u = [ [ u | x <- [0..size-1] ] | y <- [0..size-1]]

testBoard = initBoard 19

