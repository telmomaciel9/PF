--50 questoes


--1


enumFromTo' :: Int -> Int ->[Int]
enumFromTo' x y 
  | x==y = [x]
  | otherwise = x : enumFromTo' (x+1) y


--2 

enumFromThenTo' :: Int -> Int-> Int -> [Int]
enumFromThenTo' x y z
  | x<=z = x : enumFromThenTo' y (y*2-x) z
  | otherwise = []

--3

concatena' :: [a] -> [a] -> [a] 
concatena' [] l = l
concatena' l [] = l 
concatena' (x:xs) l = x : concatena' xs l

--4

pos' :: [a] -> Int -> a
pos' l n
  | (length l -1)==n  = last l
  | otherwise = pos' (init l) n


--5

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs)= last (x:xs) : reverse' (init (x:xs))  


--6

take' :: Int -> [a] -> [a]
take' _ [] = []
take' x l 
  | length l==x = l
  | otherwise = take' x (init l)


--7 

drop' :: Int -> [a] -> [a]
drop' 0 l=l
drop' n [] = []
drop' n (h:t) = drop' (n-1) t 

--8

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (h1:t1) (h2:t2) = (h1,h2) : zip' t1 t2

--9

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (h:t) 
  | n==h = True
  | otherwise = elem' n t

--10

replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a: replicate' (n-1) a

--11

intersperse' :: a -> [a] ->[a]
intersperse' _ [] = []
intersperse' _ [a] = [a]
intersperse' n (h:t) = h:n: intersperse' n t


--12

mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup [a] = [[a]]
mygroup (a:b)
    | a == head (c1) = (a:c1) : c2
    | otherwise = [a]: (c1:c2)
    where 
        (c1:c2) = mygroup b
  

--13

concat' :: [[a]] -> [a]
concat' [] = []
concat' (a:b) = a ++ concat' b 


--14

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]


--15

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l : (tails' (tail l))


--16

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] l = True
isPrefixOf' l [] = False
isPrefixOf' (h1:t1) (h2:t2) 
  | h1==h2 = isPrefixOf' t1 t2
  | otherwise= False


--17

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] l = True
isSuffixOf' l [] = False
isSuffixOf' l1 l2
  | length l1 > length l2 = False
  | last l1 == last l2 = isSuffixOf' (init l1) (init l2)
  | otherwise = False   



--36 
elemMSet' ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet' a [] = False
elemMSet' a ((x,n):xs)  
  | a == x = True 
  | otherwise = elemMSet' a xs

--ou

elemMSet2 ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet2 a [] = False
elemMSet2 a ((x,n):xs) = a == x || elemMSet' a xs


--39

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,n): xs) 
  | x == a = (a,n+1) : xs
  | otherwise = (a,n) : insereMSet x xs


--40

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,n):xs)
  | x==a && n==1 = xs 
  | x==a = (a,n-1) : xs
  | otherwise = (a,n) : removeMSet x xs


--41

--constroiMSet "aaabccc" corresponde a [(’a’,3), (’b’,1), (’c’,3)].

-- 25

p25 :: Ord a => a -> [a] -> [a]
p25 x [] = [x]
p25 x (h:t)
    | x > h = h:p25 x t
    | otherwise = x:h:t

-- 26

p26 :: [String] -> String
p26 [] = ""
p26 [a] = a
p26 (h:t) = h ++ " " ++ p26 t

-- 27

p27 :: [String] -> String
p27 [] = ""
p27 (h:t) = h ++ "\n" ++ p27 t

-- 28

p28 :: Ord a => [a] -> Int
p28 [_] = 0 
p28 (h:t)
    | h > (t !! x) = 0
    | otherwise = 1 + x
    where x = p28 t

-- 29

p29 ::  Eq a => [a] -> Bool
p29 [] = False
p29 (h:t) 
  | elem h t 
  | otherwise= p29 t

-- 30

p30 :: [Char] -> [Char]
p30 [] = []
p30 (h:t)
    | elem h ['0'..'9'] = h:p30 t
    | otherwise = p30 t

-- 31

p31 :: [a] -> [a]
p31 [] = []
p31 [_] = []
p31 (h:s:t) = s:p21 t

-- 32

p32 :: [a] -> [a]
p32 [] = []
p32 [x] = [x]
p32 (h:s:t) = h:p32 t

-- 33

p33 :: Ord a => [a] -> Bool
p33 [] = True
p33 [_] = True
p33 (h:s:t) = s >= h && p33 (s:t)

-- 34

-- Função insert' definida na questão 25

p34 :: Ord a => [a] -> [a]
p34 [] = []
p34 (h:t) = p25 h (p34 t)

-- Outras formas de ordenar listas:

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (l:ls) = maisPequenos ++ [l] ++ maiores
    where maisPequenos = quickSort $ filter (<=l) ls
          maiores = quickSort $ filter (>l) ls

-- 35

p35 :: String -> String -> Bool
p35 _ "" = False
p35 (h:t) (h':t') 
  | h < h' 
  | otherwise = p35 t t'

-- 36

p36 ::  Eq a => a -> [(a,Int)] -> Bool
p36 _ [] = False
p36 a ((x,_):xs) 
  |a == x 
  | otherwise= p36 a xs

-- 37

p37 ::  [(a,Int)] -> Int
p37 [] = 0
p37 ((x,n):xs) = n + p37 xs

-- 38

p38 :: [(a,Int)] -> [a]
p38 [] = []
p38 ((x,1):xs) = x:p38 xs
p38 ((x,n):xs) = x:p38 ((x,n-1):xs)

-- 39

p39 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
p39 x [] = [(x,1)]
p39 x ((a,n):xs) 
  |x == a = (a,n+1):xs 
  | otherwise=  (a,n):p39 x xs

-- 40

p40 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
p40 x [] = []
p40 x ((a,n):xs) 
  |  x == a = xs 
  | otherwise =  (a,n):p40 x xs

-- 41

p41 :: Ord a => [a] -> [(a,Int)]
p41 [] = []
p41 (l:ls) = p39 l (p41 ls)

-- 42

partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left x):ls) = x:partitionLefts ls
          partitionLefts ((Right _):ls) = partitionLefts ls
          partitionRights [] = []
          partitionRights ((Left _):ls) = partitionRights ls
          partitionRights ((Right x):ls) = x:partitionRights ls

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a:as,bs)
    where (as,bs) = partitionEithers' t
partitionEithers' ((Right b):t) = (as,b:bs)
    where (as,bs) = partitionEithers' t  

-- 43

p43 :: [Maybe a] -> [a]
p43 [] = []
p43 (m:ms) = case m of Nothing -> p43 ms
                       Just x -> x:p43 ms

p43' :: [Maybe a] -> [a]
p43' [] = []
p43' ((Just a):ms) = a:p43' ms
p43' (Nothing:ms) = p43' ms

-- 44

data Movimento = Norte | Sul | Este | Oeste deriving Show

p44 :: (Int,Int) -> [Movimento] -> (Int,Int)
p44 p [] = p
p44 (x, y) (m:ms) = p44 (case m of Norte -> (x, y + 1)
                                       Sul -> (x, y - 1)
                                       Este -> (x + 1, y)
                                       Oeste -> (x - 1, y)) ms

-- 45

p45 :: (Int, Int) -> (Int, Int) -> [Movimento]
p35 (xi, yi) (xf, yf) | xi < xf   = Este : p45 (xi + 1, yi) (xf, yf)
                      | xi > xf   = Oeste : p45 (xi - 1, yi) (xf, yf)
                      | yi < yf   = Norte : p45 (xi, yi + 1) (xf, yf)
                      | yi > yf   = Sul : p45 (xi, yi - 1) (xf, yf)
                      | otherwise = []

-- 46

p46 :: [Movimento] -> Bool
p46 [] = True
p46 (l:ls) = case l of Este -> False
                       Oeste -> False
                       _ -> p46 ls

-- 47

data Posicao = Pos Int Int deriving Show

p47 :: [Posicao] -> Posicao
p47 [Pos x y] = Pos x y
p47 ((Pos x y):(Pos a b):ps) 
  |(x^2 + y^2) < (a^2 + b^2) = p47 (Pos x y : ps)
  | otherwise=  p47 (Pos a b : ps)

-- 48

p48 :: Posicao -> [Posicao] -> [Posicao]
p48 _ [] = []
p48 (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
                                       then Pos xv yv : p48 (Pos x y) ps 
                                       else p48 (Pos x y) ps

-- 49

p49 :: [Posicao] -> Bool
p49 [] = True
p49 [Pos _ _] = True
p49 ((Pos _ y):(Pos x2 y2):ps) = y == y2 && p49 (Pos x2 y2 : ps)

-- 50

data Semaforo = Verde | Amarelo | Vermelho deriving Show

p50 :: [Semaforo] -> Bool
p50 ss = length [s | s <- ss, case s of Vermelho -> False; _ -> True] < 2

