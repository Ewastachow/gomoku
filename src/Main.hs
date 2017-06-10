module Main where

import Data.String
import Board as Br
import Minmax as Mm

main::IO()
main = undefined



testBoard = initBoard 19 E
a = initBoard 19 E
b = insertInBoard a 3 3 X
c = insertInBoard b 3 4 X
d = insertInBoard c 3 5 X
e = insertInBoard d 3 6 X
ee = insertInBoard d 0 0 O
eff = insertInBoard d 18 18 O
f = insertInBoard e 3 7 X
g = insertInBoard f 3 8 X
h = insertInBoard f 3 5 O
testSet = Mm.neighborhood (Br.board e)
    
-- po read zrobić instancje read jak dla show
-- dla read trzeba 
-- visual codee
