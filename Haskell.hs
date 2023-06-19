{-
-- Program koji prima listu uređenih parova mjesta na sahovskoj ploci na kojima se nalaze kraljice i provjerava jel se kraljice međusobno napadaju

f :: [(Int,Int)] -> Bool -- sve kombinacije pregledavanja
f [(x,y)] = True
f (x:y:xs) = fja x y && f (y:xs) && f (x:xs) && f xs
f [] = True

fja :: (Int,Int) -> (Int,Int) -> Bool -- Pregled redaka, stupaca i dijagonala
fja (a,b) (c,d)
     | a == c = False
     | b == d = False
     | abs(a-c) == abs(b-d) = False 
     | otherwise = True

fkonac l n = f l  
-}

{-  
Napišite funkciju octfour koja na ulazu prima prirodan broj n u dekadskom zapisu i vraća broj znamenaka 4 svih brojeva od 1 do n oktalnom zapisu (baza 8).
Oktalni brojevi od 1 do 20 su 1, 2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 22, 23, 24. Među njima postoje tri znamenke 4

dec2oct' :: Int -> [Int] -- pretvaranje dekatskog broja u oktalni
dec2oct' 0 = []
dec2oct' n = n `mod` 8 : dec2oct' (n `div` 8) 
dec2oct x = reverse (dec2oct' x)

fff (x:xs) = dec2oct x ++ fff xs
fff [] = []

fpomoc n = fff ([1..n]) -- lista znamenki svih oktalih brojeva do n

aaa :: [Int] -> Int 
aaa (x:xs) = if x == 4 then 1 + aaa xs else aaa xs
aaa [] = 0

octfour n = aaa (fpomoc n) -- prebrojava koliko znamenki 4 ima u listi
-}

{-
for a i j d | i == d = [] -- moja fja koja sluzi kao for petlja za metice u ovom slucaju izvlaci pozicije na kojima je vrijednost == 0
            | j == d = for a (i+1) 0 d 
            | a!!i!!j == 0 = [(i,j)] ++ for a i (j+1) d
            | otherwise = for a i (j+1) d
msum x = for x 0 0 (length x) 
-}
{-
decToHex' :: Int -> [Char] -- pretvaranje dekatskog broja u heksadecimalni
decToHex' 0 = []
decToHex' n | n `mod` 16 == 10 = "A" ++ decToHex' (n `div` 16)
            | n `mod` 16 == 11 = "B" ++ decToHex' (n `div` 16)
            | n `mod` 16 == 12 = "C" ++ decToHex' (n `div` 16)
            | n `mod` 16 == 13 = "D" ++ decToHex' (n `div` 16)
            | n `mod` 16 == 14 = "E" ++ decToHex' (n `div` 16)
            | n `mod` 16 == 15 = "F" ++ decToHex' (n `div` 16)
            | otherwise = show (n `mod` 16) ++ decToHex' (n `div` 16)

decToHex n = reverse (decToHex' n)
f [] = []
f (x:xs) = decToHex x ++ f xs
lista n = [1..n]

count (x:xs) = if x == 'A' then 1 + count xs else 0 + count xs -- prebrojava "A"
count [] = 0

aInHex n = count (f (lista n)) -- prebrojava koliko ima "A" u 1 do n heksadecimanih brojeva 
-}

{-
import Data.Char

-- Generalizirana Cezarova šifra za velika slova engleske abecede

slovo a b = chr(mod (ord a + ord b) 26 + ord 'A') 

popunik m x b 
    | length m == b = []
    | otherwise = [x!!(mod b (length x))] ++ popunik m x (b+1)

kluc x y = popunik x y 0

fja :: String -> String -> String
fja (x:xs) (y:ys) = [slovo x y] ++ fja xs (kluc xs ys)
fja [] _ = []
-}
{-
-- definiranje operacija nad razlomcima (* + - :)
data Frac = Num Int Int | NaN
               deriving (Eq)

instance Show Frac where
    show NaN = "NaN"
    show (Num a b)
        | b == 0 = "NaN"
        | a == 0 = "0"
        | b == 1 = show a
        | a `mod` b == 0 = show (a `div` b)
        | otherwise = show a ++ "/" ++ show b
    
(.+) :: Frac -> Frac -> Frac
NaN .+ _ = NaN
_ .+ NaN = NaN
(Num a b) .+ (Num c d) = (Num (a * d + b * c) (b * d))

(.-) :: Frac -> Frac -> Frac
NaN .- _ = NaN
_ .- NaN = NaN
(Num a b) .- (Num c d) = (Num (a * d - b * c) (b * d))

(.*) :: Frac -> Frac -> Frac
NaN .* _ = NaN
_ .* NaN = NaN
(Num a b) .* (Num c d) = (Num (a * c) (b * d))

(./) :: Frac -> Frac -> Frac
NaN ./ _ = NaN
_ ./ NaN = NaN
(Num a b) ./ (Num c d) 
    | c * d == 0 = NaN
    | otherwise = (Num (a * d) (b * c))
-}
{-
-- definiranje operacija nad kompleksnim brojevima (* + - :)

data Complex = Num Float Float | NaN
               deriving (Eq) 

instance Show Complex where
    show (Num a b) | a /= 0 && b == 0 = show a
                   | a == 0 && b /= 0 = show b ++ "i"
                   | a == 0 && b == 0 = "0"
                   | otherwise = show a ++ " + " ++ show b ++ "i"
    show NaN = "NaN"
    
(.+) :: Complex -> Complex -> Complex
_ .+ NaN = NaN
NaN .+ _ = NaN
(Num a b) .+ (Num c d) = (Num (a + c) (b + d)) 

(.-) :: Complex -> Complex -> Complex
_ .- NaN = NaN
NaN .- _ = NaN
(Num a b) .- (Num c d) = (Num (a - c) (b - d)) 

(.*) :: Complex -> Complex -> Complex
_ .* NaN = NaN
NaN .* _ = NaN
(Num a b) .* (Num c d) = (Num (a * c - b * d) (a * d + b * c))

(.//) :: Complex -> Complex -> Complex
_ .// NaN = NaN
NaN .// _ = NaN
(Num a b) .// (Num c d) 
    | c + d == 0 = NaN
    | otherwise = (Num (a / (c + d)) (b / (c + d)))

(./) :: Complex -> Complex -> Complex
_ ./ NaN = NaN
NaN ./ _ = NaN
(Num a b) ./ (Num c d) = ((Num a b) .* (Num c (-d))) .// ((Num c d) .* (Num c (-d)))
-}

{- 
-- preovjera pobjednika u x-oks igrici 
checkxo :: [String] -> Char
checkxo (x : xs) | (x == "XXX") = 'X'
                 | (x == "OOO") = 'O'
                 | (head xs == "XXX") = 'X'
                 | (head xs == "OOO") = 'O'
                 | (last xs == "XXX") = 'X'
                 | (last xs == "OOO") = 'O'
                 | (head x == 'X' && head (head xs) == 'X' && head(last xs) == 'X') = 'X'
                 | (head x == 'O' && head (head xs) == 'O' && head(last xs) == 'O') = 'O'
                 | (x !! 1 == 'X' && (head xs) !! 1 == 'X' && (last xs) !! 1 == 'X') = 'X'
                 | (x !! 1 == 'O' && (head xs) !! 1 == 'O' && (last xs) !! 1 == 'O') = 'O'
                 | (x !! 2 == 'X' && (head xs) !! 2 == 'X' && (last xs) !! 2 == 'X') = 'X'
                 | (x !! 2 == 'O' && (head xs) !! 2 == 'O' && (last xs) !! 2 == 'O') = 'O'
                 | (x !! 0 == 'X' && (head xs) !! 1 == 'X' && (last xs) !! 2 == 'X') = 'X'
                 | (x !! 2 == 'X' && (head xs) !! 1 == 'X' && (last xs) !! 0 == 'X') = 'X'
                 | (x !! 2 == 'O' && (head xs) !! 1 == 'O' && (last xs) !! 0 == 'O') = 'O'
                 | (x !! 0 == 'O' && (head xs) !! 1 == 'O' && (last xs) !! 2 == 'O') = 'O'
                 | otherwise = 'N'
-}

{- 
-- Zbrajanje 2 binarna broja 
i2b :: Int -> [Char]
i2b 0 = []
i2b n = if (mod n 2 == 0) then "0" ++  i2b(n`div` 2)  else "1" ++ i2b((n-1) `div` 2)

fja3 :: [Char] -> Int -> [Char] 
fja3 x n =x ++ replicate n '0'

sumb2 :: [Char] -> [Char] -> Char -> [Char]
sumb2 [] [] x = if (x == '1') then "1" else []
sumb2 (x : xs) (y : ys) a | (x == '0') && (y == '0') && (a == '0') = "0" ++ sumb2 xs ys '0'
                          | (x == '1') && (y == '0') && (a == '0') = "1" ++ sumb2 xs ys '0'
                          | (x == '0') && (y == '1') && (a == '0') = "1" ++ sumb2 xs ys '0'
                          | (x == '0') && (y == '0') && (a == '1') = "1" ++ sumb2 xs ys '0'
                          | (x == '1') && (y == '1') && (a == '0') = "0" ++ sumb2 xs ys '1'
                          | (x == '0') && (y == '1') && (a == '1') = "0" ++ sumb2 xs ys '1'
                          | (x == '1') && (y == '0') && (a == '1') = "0" ++ sumb2 xs ys '1'
                          | otherwise = "1" ++ sumb2 xs ys '1'

sumb m n = if (length m > length n) then reverse(sumb2 m (fja3 n (length m - length n)) '0') else reverse(sumb2 n (fja3 m (length n - length m)) '0' )
-}

{- 
-- funkcija euclides koja traži najveći pozitivni zajednički djelitelj cijelih brojeva a i b pomoću Euklidovog algoritma. 

euclid :: Int -> Int -> Int 
euclid x 0 = x
euclid x y = euclid y (x `mod` y)
-}

