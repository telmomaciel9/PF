--1
p1 :: Int -> Int -> [Int]
p1 x y 
  | x>y = []
  | otherwise=  x : p1 (x+1) y

p1' :: Int -> Int -> [Int]
p1' x y 
  | x<y =  x : p1 (x+1) y
  | x==y = [x]
  | otherwise= []

--2
p2' :: Int -> Int -> Int -> [Int]
p2' x y z 
    | x > z = if y > z then x:p2' (y) (y-(x-y)) z else [x]
    | x < z = if y < z then x:p2' (y) (y+(y-x)) z else [x]
    | x == z =  [x]

{-
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x  y z
    | x > z 
         | y > x = []
         | y > z = x:myenumFromThenTo (y) (y-(x-y)) z
         | y < z = [x] 
         | otherwise=  x:[y]
    | x < z 
         |y < x = []
         |y < z = x:myenumFromThenTo (y) (y+(y-x)) z 
         |y== z = x:[y] 
         | otherwise = [x]
    | x == z =  []
-}

--3
p3 :: [a] -> [a] -> [a]
p3 [] l = l
p3 (h:t) l = h : p3 t l

--4
p4 :: [a] -> Int -> a
p4 l 0 = head l
p4 (h:t) n = p4 t (n-1)

pos' :: [a] -> Int -> a
pos' l n
  | (length l -1)==n  = last l
  | otherwise = pos' (init l) n

--5
p5 :: [a] -> [a]
p5 [] = []
p5 (h:t) = p5 t ++ [h]

--6
p6 :: Int -> [a] -> [a]
p6 n [] = []
p6 0 l = []
p6 n (h:t) =  h : p6 (n-1) t

--7
p7 :: Int -> [a] -> [a]
p7 _ [] = []
p7 0 l = l
p7 n (h:t) = p7 (n-1) t 

--8
p8 :: [a] -> [b] -> [(a,b)]
p8 _ [] = []
p8 [] _ = []
p8 (h:t) (h2:t2) = (h,h2): p8 t t2

--9
p9 :: Eq a => a -> [a] -> Bool
p9 _ [] = False
p9 a (h:t) 
  | a==h = True
  | otherwise= p9 a t

--10
p10 :: Int -> a -> [a]
p10 0 _ = []
p10 n l = l : p10 (n-1) l

--11
p11 :: a -> [a] -> [a]
p11 _ [] = []
p11 _ [a] = [a]
p11 n (h:t) = h:n: p11 n t

--12
p12 :: Eq a => [a] -> [[a]]
p12 [] = []
p12 l = aux l: p12 restos
   where restos = drop (length (aux l)) l

aux [] = []
aux [a]= [a]
aux (h1:h2:t) = if h1 == h2 then h1 : aux (h2:t) else [h1]

--13
p13 :: [[a]] -> [a]
p13 [] = []
p13 (h:t) = h ++ p13 t

--14
p14 :: [a] -> [[a]]
p14 [] = [[]]
p14 l = p14 (init l) ++ [l]

--15
p15 :: [a] -> [[a]]
p15 [] = [[]]
p15 l = l : p15 (tail l)

--16
p16 :: Eq a => [a] -> [a] -> Bool
p16 [] _ = True
p16 _ [] = False
p16 (h:t) (h1:t1) 
  | h==h1 = p16 t t1
  | otherwise = False

--17
p17 :: Eq a => [a] -> [a] -> Bool
p17 [] _ = True
p17 _ [] = False
p17 l1 l2 
  | (last l1) == (last l2) = p17 (init l1) (init l2)
  | otherwise= False

--18
p18 :: Eq a => [a] -> [a] -> Bool
p18 [] _ = True
p18 _ [] = False
p18 (h1:t1) (h2:t2)
  | h1==h2 = p18 t1 t2
  | otherwise = p18 (h1:t1) t2

--19 
p19 :: Eq a => a -> [a] -> [Int]
p19 _ [] = []
p19 x l
  | last l == x = p19 x (init l)++[(length l)-1]
  | otherwise = p19 x (init l)

--20
p20 :: Eq a => [a] -> [a]
p20 [] = []
p20 (h:t)
  | elem h t = p20 t
  | otherwise = h: p20 t

--21
p21 :: Eq a => a -> [a] -> [a]
p21 _ [] = []
p21 n (h:t)
  | n==h = t
  | otherwise= h: p21 n t

--22
p22 :: Eq a => [a] -> [a] -> [a]
p22 l [] = l
p22 [] _ = []
p22 l (h:t) = p22 (p21 h l) t

p22' :: Eq a => [a] -> [a] -> [a]
p22' l [] = l
p22' [] _ = []
p22' (h:t) (a:b)
  | h==a = p22' t b
  | otherwise= h: p22' t (a:b)

--23
p23 :: Eq a => [a] -> [a] -> [a]
p23 l [] = l
p23 l (h:t)
    | elem h l = p23 l t
    | otherwise = p23 (l ++ [h]) t

--24
p24 :: Eq a => [a] -> [a] -> [a]
p24 [] _ = []
p24 l [] = l
p24 (h:t) l 
  | elem h l = h: p24 t l
  | otherwise = p24 t l

--25
p25 :: Ord a => a -> [a] -> [a]
p25 n [] = [n]
p25 n (h:t)
  | n<=h = n:h:t
  | otherwise= h: p25 n t

--26
p26 :: [String] -> String
p26 [] = ""
p26 [a] = a
p26 (h:t) = h ++ " " ++ p26 t

--27
p27 :: [String] -> String
p27 [] = ""
p27 (h:t)= h ++ "\n" ++ p27 t

myunlines :: [String] -> String
myunlines [] = []
myunlines ([]:c) = '\n':myunlines c
myunlines ((a:b):c) = a:myunlines (b:c)

--28
p28 :: Ord a => [a] -> Int
p28 [a] = 0
p28 (h:t)
  | h> (t!!(p28 t)) = 0
  | otherwise= 1+ p28 t

--29
p29 ::Eq a=> [a] -> Bool
p29 [] = False
p29 (h:t)
  | elem h t = True
  | otherwise= p29 t

--30
p30 :: [Char] -> [Char]
p30 [] = []
p30 (h:t)
  | elem h ['0'..'9']= h: p30 t
  | otherwise = p30 t

--31
p31 :: [a] -> [a]
p31 [] = []
p31 [a] = []
p31 (h1:h2:t) = h2: p31 t

--32
p32 :: [a]->[a]
p32 [] =[]
p32 [a] = [a]
p32 (h1:h2:t) = h1 : p32 t

--33
p33 :: Ord a => [a] -> Bool
p33 [] = True
p33 [_] = True
p33 (h:x:t)
  | h<=x = p33 (x:t)
  | otherwise = False

--34
p34 :: Ord a => [a] -> [a]
p34 [] =[]
p34 (h:t) = p25 h (p34 t)

--35
p35 :: String -> String -> Bool
p35 _ "" = False
p35 "" _ = True
p35 (h1:t1) (h2:t2)
  | h1<h2 = True
  | h1>h2 = False
  | otherwise = p35 t1 t2

--36
p36 :: Eq a=> a -> [(a,Int)] -> Bool
p36 _ [] = False
p36 n ((a,b):t)
  | n==a = True
  | otherwise= p36 n t

--37
p37 :: [(a,Int)] -> Int
p37 [] = 0
p37 ((a,b):t) = b+ p37 t

--38
p38 :: [(a,Int)] -> [a]
p38 [] = []
p38 ((a,1):t) = a:p38 t
p38 ((a,b):t)= a:p38 ((a,b-1):t)

--39
p39 :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
p39 a [] = [(a,1)]
p39 x ((a,n):t)
  | x==a = (a,n+1):t
  |otherwise= (a,n): p39 x t

--40
p40 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
p40 a [] = []
p40 x ((a,b):t) 
  | x==a && b==1 = t
  | x==a = (a,b-1):t
  | otherwise= (a,b): p40 x t

--41
p41 :: Ord a => [a] -> [(a,Int)]
p41 [] = []
p41 (l:ls) = p39 l (p41 ls)

--42
p42 ::  [Either a b] -> ([a],[b])
p42 l = (pLefts l, pRights l)

pLefts [] = []
pLefts ((Left x):ls) = x:pLefts ls
pLefts ((Right _):ls) = pLefts ls

pRights [] = []
pRights ((Left _):ls) = pRights ls
pRights ((Right x):ls) = x:pRights ls

--43
p43 :: [Maybe a] -> [a]
p43 [] = []
p43 (h:t) = case h of Nothing -> p43 t
                      Just a -> a:p43 t

--44
data Movimento = Norte | Sul | Este | Oeste deriving Show

p44 :: (Int,Int) -> [Movimento] -> (Int,Int)
p44 c [] = c
p44 (x,y) (h:t) = case h of Norte -> p44 (x,y+1) t
                            Sul -> p44 (x,y-1) t
                            Este -> p44 (x+1,y) t
                            Oeste -> p44 (x-1,y) t 

--45
p45 :: (Int,Int) -> (Int,Int) -> [Movimento]
p45 (a,b) (x,y)
   | a<x = Este : p45 (a+1,b) (x,y)
   | a>x = Oeste : p45 (a-1,b) (x,y)
   | b<y = Norte : p45 (a,b+1) (x,y)
   | b>y = Sul : p45 (a,b-1) (x,y)
   |otherwise = []

--46
p46 :: [Movimento] -> Bool
p46 [] = True
p46 (h:t) = case h of Norte -> p46 t
                      Sul -> p46 t
                      Este -> False
                      Oeste -> False

--47
data Posicao = Pos Int Int deriving Show

p47 :: [Posicao] -> Posicao
p47 [a] = a
p47 ((Pos x y):(Pos a b):ps) 
  |(x^2 + y^2) < (a^2 + b^2) = p47 ((Pos x y) : ps)
  | otherwise=  p47 ((Pos a b) : ps)

--48
p48 :: Posicao -> [Posicao] -> [Posicao]
p48 _ [] = []
p48 (Pos x y) ((Pos a b):ps) 
  |abs (x-a) == 1 && y == b = Pos a b : p48 (Pos x y) ps
  |abs (y-b) == 1 && x == a = Pos a b : p48 (Pos x y) ps 
  |otherwise= p48 (Pos x y) ps

--49
p49 :: [Posicao] -> Bool
p49 [] = True
p49 [_]= True
p49 ((Pos x y):(Pos a b):t) = y==b && p49 ((Pos a b):t)

p49' :: [Posicao] -> Bool
p49' [] = True
p49' [_]= True
p49' ((Pos x y):(Pos a b):t)
  | y==b = p49' ((Pos a b):t)
  |otherwise = False

--50
data Semaforo = Verde | Amarelo | Vermelho deriving Show

p50 :: [Semaforo] -> Bool
p50 l 
  | (length (removeVermelho l)) < 2 = True
  | otherwise= False 

removeVermelho :: [Semaforo] -> [Semaforo]
removeVermelho [] = []
removeVermelho (Vermelho:t) = removeVermelho t
removeVermelho (h:t) = h : removeVermelho t