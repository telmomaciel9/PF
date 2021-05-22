

--Exercicio 1

--a
any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs)
  | p x = True
  | otherwise= any' p xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' p [] = False
any'' p (x:xs)= p x || any'' p xs

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = False
all' p (x:xs)= p x && all' p xs

--c
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' _ [] =[]
takeWhile' p (x:xs)
  | p x = x: takeWhile' p xs
  | otherwise= []

--d
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' _ [] =[]
dropWhile' p (x:xs)
  | p x= dropWhile' p xs
  |otherwise= (x:xs)

--e
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' p (x:xs) 
  | not (p x) = ([],(x:xs))
  | p x = let (e,d)= span' p xs
          in (x:e,d)

--f
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy p x [] = []
deleteBy p x (y:ys)
  | p x y = ys
  | otherwise= y : deleteBy p x ys

--g
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:xs) = insert x (sortOn' f xs)
   where insert x [] = [x]
         insert x (y:ys)
            | f x< f y= x:y:ys
            |otherwise=y: insert x ys

--------------

--Exercicio 2
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a
selgrau :: Int -> Polinomio -> Polinomio
selgrau g p = filter (\(c,e) -> e==g) p

--b
conta :: Int -> Polinomio -> Int
conta g p = sum (map (\(c,e)-> if e==g then 1 else 0 ) p)

conta1 :: Int -> Polinomio -> Int
conta1 g p = foldr aux 0 p
   where aux :: Monomio -> Int -> Int
         aux (c,e) n = if e==g then 1+n else n

--c
grau :: Polinomio -> Int
grau p = maximum (map snd p)

--d
deriv :: Polinomio -> Polinomio
deriv p = let p1 = map (\(c,e)->(c*(fromIntegral e),e-1)) p
          in filter (\(c,e)->c/=0) p1

--e
calcula :: Float -> Polinomio -> Float
calcula v p= sum (map (\(c,e)-> c*(v^e)) p)

calcula1 v p = foldr aux 0 p
   where aux :: Monomio -> Float -> Float
         aux (c,e) r = c*(v^e) + r 

--g
mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) p = map (\(c1,e1)-> (c1*c,e1+e)) p

--h
ordena :: Polinomio -> Polinomio
ordena [] =[]
ordena ((c,e):p) = ordena lmen ++ [(c,e)] ++ ordena lmai
    where lmen = filter (\(c1,e1) -> e1<=e ) p
          lmai = filter (\(c1,e1) -> e1>e ) p 

ordena1 :: Polinomio -> Polinomio
ordena1 p = sortOn' snd p

--Exercicio 3

type Mat a = [[a]]

--a
dimOK :: Mat a -> Bool
dimOK [] = True
dimOK (l1:l2:m) = length l1 == length l2 && dimOK (l2:m)

dimOK' :: Mat a -> Bool
dimOK' [l] = True
dimOK' (l:m) = let n = length l
               in all (\l1 -> length l1 == n) m 

--c
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (l1:m1) (l2:m2) =
   (zipWith (+) l1 l2) : addMat m1 m2

addMat' :: Num a => Mat a -> Mat a -> Mat a
addMat' [] [] = []
addMat' m1 m2 = zipWith (zipWith (+)) m1 m2

--f

mapWMat :: (a -> b ) -> Mat a -> Mat b 
mapWMat f m = map (\l -> map f l) m

mapWMat' :: (a -> b ) -> Mat a -> Mat b 
mapWMat' f m = map (map f) m 

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f m1 m2 = zipWith (\l1 l2 -> zipWith f l1 l2) m1 m2

zipWMat' :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat' f m1 m2 = zipWith (zipWith f) m1 m2

--d

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)

--g

triSup :: (Eq a, Num a) => Mat a -> Bool
triSup [] = True
triSup (l:m) = (all (==0) (map head m)) && triSup (map tail m)

triSup' :: (Eq a, Num a) => Mat a -> Bool
triSup' [] = True
triSup' m = all (==0) (aux 0 m)
  where aux :: Int -> Mat a -> [a]
        aux _ [] = []
        aux k (l:m)= take k l ++ aux (k+1) m