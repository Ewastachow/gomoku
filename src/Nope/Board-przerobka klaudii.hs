--newtype Plansza
--data Pole

--instancje Show dla wyswietlania w ghci
--metoda Read - 2 instancje dla typów wbudowanych
--Show -> String -> Read

--wyswietlenie wszystkich potencjalnych ruchów gracza
--funkcja kosztu wyliczająca wartość planszy ( uwzględniająca pozycję na planszy)
--algorytm /WhyI?/ costam na podstawie drzewa
--z kazdym mozliwym ruchem wartość punktowa ruchu (fcja oceniająca)
--realworld haskell
-- patrtition	



data Piece = E | O | X deriving Eq

newtype Board = Board { board::[[Piece]] } deriving Show

instance Show Piece where
	show E = "_"
	show O = "O"
	show X = "X"

--instance Show Board where
--	

------Init Board------

initBoard size u = Board [ [ u | x <- [0..size-1] ] | y <- [0..size-1]]
initNewBoard size = initBoard size E

------Show Board------

showLine :: [Piece] -> String
showLine [] = ""
showLine (elem:reszta_elem) = 
	show elem ++ " " ++ showLine reszta_elem

showLineLn it xs 
    | it < 10 = " " ++ show it ++ ":  " ++ showLine xs ++ "\n"
    | it >= 10 = show it ++ ":  " ++ showLine xs ++ "\n"


------Testowe------
testBoard = initNewBoard 4

testRow = replicate 4 X



