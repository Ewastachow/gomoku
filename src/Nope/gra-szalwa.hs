import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Maybe
import Data.Tree as Tree

data Color = B | C

data Wspolrzedne = Wspolrzedne Int Int deriving (Eq, Show)

instance Show Color where
   show B = "X"
   show C = "O"

data Plansza = Plansza (M.Map Wspolrzedne Color) 


instance Show Plansza where
    show = toString

instance Ord Wspolrzedne where
   (Wspolrzedne a b) `compare` (Wspolrzedne c d)
       | a == c = b `compare` d
       | otherwise = a `compare` c

toString :: Plansza -> String
toString(Plansza map) = 
   przejdzPlansze([Wspolrzedne a b | b <- [1..19], a <- [1..19]]) where
      przejdzPlansze[x] = (wyswietlWspolrzedne x)
      przejdzPlansze(x:xs) = (wyswietlWspolrzedne x) ++ przejdzPlansze(xs)
      wyswietlWspolrzedne :: Wspolrzedne -> String
      wyswietlWspolrzedne (Wspolrzedne a b)
         | a == 19 && M.member (Wspolrzedne a b) map = show(fromJust(M.lookup (Wspolrzedne a b) map)) ++ "|\n"
         | a == 1 && M.member (Wspolrzedne a b) map = "|" ++ show(fromJust(M.lookup (Wspolrzedne a b) map))
         | M.member (Wspolrzedne a b) map = show(fromJust(M.lookup (Wspolrzedne a b) map))
         | a == 19 = " |" ++ "\n"
         | a == 1 = "| "
         | otherwise = " "

-- m = M.fromList [(Wspolrzedne 1 1, B), (Wspolrzedne 2 2, C), (Wspolrzedne 1 3, B), (Wspolrzedne 2 1, C)]
-- p = Plansza m


wprowadzKolor a b color (Plansza map)
   | M.notMember (Wspolrzedne a b) map = Plansza (M.insert (Wspolrzedne a b) color map)
   | otherwise = Plansza map

zmienKolor B = C
zmienKolor C = B

czyWolnePole a b color (Plansza p)
   -- | M.notMember (Wspolrzedne a b) p = [wprowadzKolor a b color (Plansza p)]
   | M.notMember (Wspolrzedne a b) p = [stworzDrzewo color (wprowadzKolor a b color (Plansza p))]
   | otherwise = []

stworzPodrzewo a b color (Plansza p)
   | a > 0 && a < 19 && b > 0 && b <= 19 = (czyWolnePole a b color (Plansza p)) ++ stworzPodrzewo(a + 1) b color (Plansza p)
   | a == 19 && b > 0 && b <= 19 = (czyWolnePole a b color (Plansza p)) ++ stworzPodrzewo 1 (b + 1) color (Plansza p)
   | otherwise = []

-- stworzDrzewo a tablica = Node {rootLabel = a, subForest = tablica}

stworzDrzewo color (Plansza p) = Node {rootLabel = (Plansza p), subForest = (stworzPodrzewo 1 1 color (Plansza p)) }



