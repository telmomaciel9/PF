module FichasAll where

-- Exercicio 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)


p::Polinomio
p=[(4,5),(2,1),(7,0),(3,2),(1,1),(1,5)]

--Alinea C

selgrau :: Int -> Polinomio -> Polinomio
selgrau g [] = []
selgrau g ((c,e):p)
  | g==e = (c,e): selgrau g p
  | otherwise = selgrau g p

--Alinea B

grau :: Polinomio -> Int
grau [(c,e)]= e
grau ((c1,e1):(c2,e2):p) 
  | e1>e2 = grau ((c1,e1):p)
  | otherwise = grau ((c2,e2):p)   

--Alinea D

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,e):p) = (c*(fromIntegral e),e-1): deriv p

--Alincea E

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula v ((c,e):p) = c*(v^e) + calcula v p

--Alinea G

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (c,e) ((c1,e1):p) = (c*c1,e+e1) : mult (c,e) p

--Alinea H

normaliza :: Polinomio ->Polinomio
normaliza [] = []
normaliza ((c,e):p) =
  let p1 = selgrau e ((c,e):p)
      c1 = somaCoef p1
      p2 = semGrau e p
  in (c1,e) : normaliza p2

somaCoef :: Polinomio -> Float
somaCoef [] = 0
somaCoef ((c,e):p) = c + somaCoef p

semGrau :: Int -> Polinomio -> Polinomio
semGrau g [] = []
semGrau g ((c,e):p)
  | g/=e = (c,e): semGrau g p
  | otherwise = semGrau g p

--se o polinomio tiver ordenado

normaliza' :: Polinomio-> Polinomio
normaliza' ((c1,e1):(c2,e2):p)
  | e1/=e2 = (c1,e1) : normaliza' ((c2,e2):p)
  | e1==e2 = normaliza' ((c1+c2,e1):p)
normaliza' p = p

--Alinea I

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

-- polinomios normalizados e ordenados

soma' :: Polinomio -> Polinomio -> Polinomio
soma' [] p = p 
soma' p [] = p
soma' ((c1,e1):p1) ((c2,e2):p2) 
  | e1 < e2 = (c1,e1) : soma' p1 ((c2,e2):p2)
  | e1 == e2 = if c1+c2 /=0 
               then (c1+c2,e1) : soma' p1 p2
               else soma' p1 p2
  | e1 > e2 = (c2,e2) : soma' ((c1,e1):p1) p2
 
--Alinea J

produto :: Polinomio -> Polinomio -> Polinomio
produto _ [] = []
produto [] _ = []
produto (m:p1) p2 = (mult m p2) ++ produto p1 p2

produtoF :: Polinomio -> Polinomio -> Polinomio
produtoF p1 p2 = normaliza (produto p1 p2)

--Alinea K

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:p) = insere m (ordena p)
 where 
  insere :: Monomio -> Polinomio -> Polinomio
  insere (c,e) [] = [(c,e)]
  insere (c,e) ((c1,e1):p)
    | e>e1 = (c1,e1): insere (c,e) p
    | e==e1 = (c+c1, e):p
    | e<e1 = (c,e):(c1,e1):p

--Alinea L

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)


-- Exercicio 2

type Poligonal = [Ponto]
--copiar exercicio 6 da ficha 1--


--a
comprimento:: Poligonal -> Double
comprimento _ [] = 0
comprimento (p1:p2:lp)= (dist p1 p2) + comprimento (p2:lp)

--c
-- fechada + de 2 pontos
triangula :: Poligonal -> [Figura]
triangula lp = parte (init lp)
  where parte (p1:p2:p3:lp)= (Triangulo p1 p2 p3): parte (p1:p3:lp)
        parte _ = []








-- Exercicio 3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

ag = [("A",[Casa 9999, Email "axxxxx"]),("B",[Trab 8888, Casa 7777])]

-- Alinea a

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n em [] = [(n,[Email em])]
acrescEmail n em ((n1,lc):ag) 
  | n==n1 = ((n1,(Email em):lc):ag)
  | n/=n1 = (n1,lc) : acrescEmail n em ag

-- Alinea b

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((n1,lc):ag)
  | n==n1 = Just (getEmail lc) 
  | n/=n1 = verEmails n ag

getEmail :: [Contacto] -> [String]
getEmail [] = []
getEmail ((Email e):lc) = e : getEmail lc
getEmail (_ : lc) = getEmail lc


-- Alinea c

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa t):lc) = t : consTelefs lc
consTelefs ((Trab t):lc) = t : consTelefs lc
consTelefs ((Tlm t):lc) = t : consTelefs lc
consTelefs (_:lc) = consTelefs lc


-- Alinea d

casa :: Nome -> Agenda -> Maybe Integer
casa n [] =   Nothing
casa n ((n1,lc):ag) 
  | n==n1 = getCasa lc
  | n/=n1 = casa n ag

getCasa :: [Contacto] -> Maybe Integer
getCasa [] = Nothing
getCasa ((Casa t):lc) = Just t
getCasa (_:lc) =  getCasa lc    

--Exercicio 4

{- Mesmos types do exercicio 3, NOME
type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String
data Data = D Dia Mes Ano  deriving Show
type TabDN = [(Nome,Data)]

tdn = [("AA", D 1 1 2002),("BB", D 3 10 1970),("CC", D 4 2 1999)]

procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing
procura n ((n1,d1):t) 
  | n==n1 = Just d1
  | otherwise = procura n t

idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D d m a) no t =
  let d1 = procura no t
  in case d1 of
    Nothing -> Nothing
    (Just (D di me an)) -> if (m,d) < (me, di) then Just (a-an-1) 
                                               else Just (a-an)

anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2) = (a1,m1,d1) < (a2,m2,d2)                                            

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):t) = insere (n,d) (ordena t)
   where
  insere :: (Nome, Data) -> TabDN -> TabDN
  insere (n,d) [] = [(n,d)]
  insere (n,d) ((n1,d1):t)
    | anterior d d1 = (n,d):(n1,d1):t
    | otherwise = (n1,d1) : insere (n,d) t

-}


-- Exercicio 5

data Movimento = Credito Float | Debito Float
              deriving Show
data Data = D Int Int Int
              deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show


ext = Ext 1000 [(D 1 1 2000, "AA", Debito 50),
                (D 1 2 2000, "BB", Credito 150),
                (D 1 3 2001, "CC", Debito 100)]


--a
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ lm) v = filtraMov lm v

filtraMov :: [(Data, String, Movimento)] -> Float -> [Movimento]
filtraMov [] _ = []
filtraMov ((_,_,Debito x):t) v
  | x>v = (Debito x) : filtraMov t v
  | otherwise= filtraMov t v
filtraMov ((_,_,Credito x):t) v
  | x>v = (Credito x) : filtraMov t v
  | otherwise= filtraMov t v


filtraMov' :: [(Data, String, Movimento)] -> Float -> [Movimento]
filtraMov' [] _ =[]
filtraMov' ((_,_,m):t) v =
  case m of
    (Debito x)-> if x>v then m:filtraMov' t v 
                        else filtraMov' t v
    (Credito x)-> if x>v then m:filtraMov' t v 
                        else filtraMov' t v


--b
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext vi lm) ld = aux ld lm
  where aux :: [String] -> [(Data, String, Movimento)] -> [(Data,Movimento)] 
        aux ls [] = []
        aux ls ((d,s,m):t) 
          | elem s ls = (d,m): aux ls t  
          | otherwise = aux ls t

--ou 


filtro' :: Extracto -> [String] -> [(Data,Movimento)]
filtro' (Ext vi lm) ld = aux ld lm
  where aux :: [String] -> [(Data, String, Movimento)] -> [(Data,Movimento)] 
        aux [] _ = []
        aux (s1:ls) lm = (procura s1 lm) ++ aux ls lm
        procura :: String -> [(Data, String, Movimento)] -> [(Data,Movimento)]
        procura s [] = []
        procura s ((d,s1,m):t)
          | s==s1 = [(d,m)]
          | otherwise = procura s t

--c
creDeb :: Extracto -> (Float,Float)
creDeb (Ext vi lm) = parte lm
  where parte :: [(Data,String,Movimento)] -> (Float,Float)
        parte [] = (0,0)
        parte ((_,_,m):t) =
          let (sc,sd) = parte t
          in case m of  
            (Credito x) -> (x+sc,sd)
            (Debito x) -> (sc, x+sd)

--d
saldo :: Extracto -> Float
saldo e@(Ext vi lm) = let (sc,sd) = creDeb e 
                      in vi+sc-sd  



{-
-- Exercicio 2

-- a)
[2^x | x<- [0..10]]

-- b)
[(x,y) | x<- [1..5], y<- [1..5], x+y==6]

-- c)
[[1..x]| x<-[1..5]]

-- d)
[ [y^0 | y<-[1..x]] | x<-[1..5]]
[ replicate x 1 | x<- [1..5]]
[ [ 1| y<-[1..x]] | x<-[1..5]]

-- e)
[ product [1..x] | x<-[1..6]]

-}


-- Exercicio 3

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) =
    let (ll,ld)= digitAlpha xs
    in if isDigit x then (ll,x:ld)
        else if isAlpha x then (x:ll,ld) 
            else (ll,ld)

--ou 

digitAlpha' :: String -> (String,String)
digitAlpha' s = separa s ([],[])
 where separa :: String -> (String,String) -> (String,String)
       separa [] (ll,ld) = (ll,ld)
       separa (x:xs) (ll,ld)
         | isDigit x = separa xs (ll,ld++[x])
         | isAlpha x = separa xs (ll++[x],ld)
         | otherwise = separa xs (ll,ld)

--Exercicio 4

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs)
    | x<0 = (nn+1, nz,np)
    | x==0 = (nn,nz+1,np)
    | x>0 = (nn,nz,np+1)
   where (nn,nz,np)= nzp xs

--ou

nzp' ::  [Int] -> (Int,Int,Int)
nzp' l = conta l (0,0,0)
    where conta :: [Int] -> (Int,Int,Int) -> (Int,Int,Int)
          conta [] (nn,nz,np) = (nn,nz,np)
          conta (x:xs) (nn,nz,np) 
                  | x<0 = conta xs (nn+1, nz,np)
                  | x==0 = conta xs (nn,nz+1,np)
                  | x>0 = conta xs (nn,nz,np+1)


-- Exercicio 6

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (x:xs) = calcula (x:xs) (length xs)
    where   calcula [x] _ = x
            calcula (y:ys) k = y*10^k + calcula ys (k-1)   

-- ou

fromDigits' :: [Int] -> Int
fromDigits' l = conv (reverse l)
    where   conv :: [Int] -> Int
            conv [] = 0
            conv (x:xs) = x + 10*(conv xs)

-- acumuladores

fromDigits2 :: [Int] -> Int
fromDigits2 l = conv l 0
    where   conv [] s = s
            conv (x:xs) s = conv xs (x+10*s)



-- Exercicio 7

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = aux l 0 0
  where aux :: (Num a, Ord a) => [a] -> a -> a -> a
        aux [] m s = m
        aux (h:t) m s 
           |(s+h) > m = aux t (s+h) (s+h)
           |otherwise = aux t m (s+h)   



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




data BTree a = Empty
             | Node a (BTree a) (BTree a)
         deriving Show

t= (Node 10 (Node 7 (Node 1 Empty Empty)(Node 4 Empty Empty))
            (Node 3 Empty (Node 5 Empty Empty)) )

--b

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = 1+ (contaNodos e) + (contaNodos d)

--c

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = (folhas e) + (folhas d)

--d

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node x e d) =Node x (prune (n-1) e) (prune (n-1) d)

--e

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node x _ _) = [x] 
path (y:ys) (Node x e d)
  | y = x : path ys d
  | not y = x : path ys e

--f

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d) (mirror e)

--g

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e1 d1) (Node y e2 d2) = 
    Node (f x y) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

--h

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (x,y,z) e d) = 
    let (a1,b1,c1)= unzipBT e
        (a2,b2,c2)= unzipBT d
    in (Node x a1 a2, Node y b1 b2, Node z c1 c2)

--Exercicio 2

--a

minimo :: Ord a => BTree a -> a
minimo (Node x Empty d) = x
minimo (Node x e d) = minimo e

--b

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d

--c

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = 
    let (m,sm) = minSmin e
    in (m,(Node x sm d))

--d

remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node y e d)
  | x<y = Node y (remove x e) d
  | x>y = Node y e (remove x d)
  | otherwise= case e of Empty -> d
                         _ -> case d of Empty -> e
                                        _ -> let (m,d') = minSmin d
                                             in Node m e d'


--Exercicio 3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                    | Rep
                    | Faltou
   deriving Show
type Turma = BTree Aluno


--a

insCNum :: Numero -> Turma -> Bool
insCNum n Empty = False
insCNum n (Node (nu,_,_,_) e d)
   | n< nu = insCNum n e
   | n> nu = insCNum n d
   | n== nu = True

--b

inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (_,no,_,_) e d) =
  n==no || (inscNome n e) || (inscNome n d)

--c

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nu,no,TE,_) e d)= (trabEst e) ++ [(nu,no)] ++ (trabEst d)
trabEst (Node (nu,no,_,_) e d)= (trabEst e) ++ (trabEst d)

--d

nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota n (Node (nu,_,_,c) e d)
   | n< nu = nota n e
   | n> nu = nota n d
   | n== nu = Just c

--e

divide :: Int -> Int -> Float
divide x y = (fromIntegral x)/(fromIntegral y)

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas t = let na= contaNodos t 
                   nf= contaFaltas t
               in 100*(devide nf na)

contaFaltas :: Turma -> Int
contaFaltas Empty = 0
contaFaltas (Node (_,_,_,Faltou) e d)= 1 + contaFaltas e + contaFaltas d
contaFaltas (Node (_,_,_,_) e d)= contaFaltas e + contaFaltas d


--Exercicio 3

data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show

t= Fork (Fork (Tip 5) (Tip 7))
        (Fork (Tip 1) (Fork (Tip 3) (Tip 2)))

--a

ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d)= ltSum e + ltSum d

--b

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

--c

ltHeight :: LTree a -> Int
ltHeight (Tip x)= 1
ltHeight (Fork e d)= 1 + max (ltHeight e) (ltHeight d)

mapLT :: (a->b) -> LTree a -> LTree b
mapLT f (Tip x) = Tip (f x)
mapLT f (Fork e d) = Fork (mapLT f e) (mapLT f d)


--Exercicio 4

data FTree a b = Leaf b | No a (FTree a b) (FTree a b) 
data BTree a = Empty | Node a (BTree a) (BTree a)


t2 = No 5 (No 4 (Leaf 'c') (Leaf 'b'))
          (No 1 (No 6 (Leaf 's') (Leaf '5')) (Leaf 'h'))

--a

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No y e d) = 
     let (bt1,lt1)= splitFTree e
         (bt2,lt2)= splitFTree d
     in (Node y bt1 bt2, Fork lt1 lt2)


--b

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees _ (Tip x) = Nothing
joinTrees Empty (Fork e d) =  Nothing
joinTrees (Node y e1 d1) (Fork e d) =  
    let lt1= joinTrees e1 e
        lt2= joinTrees d1 d 
    in case lt1 of Nothing -> Nothing
                   (Just t1) -> case lt2 of Nothing -> Nothing
                                            (Just t2) -> Just (No y t1 t2)


--Exercicio 2

data RTree a = R a [RTree a] deriving Show

t3 = R 5 [R 3 [], R 4 [R 2 [], R 1 [], R 7 []]]

--a

soma :: Num a => RTree a -> a
soma (R x lrt) = x + sum (map soma lrt)

--b

altura :: RTree a -> Int
altura (R x []) = 1
altura (R x lrt)= 1 + maximum (map altura lrt)

--c

prune :: Int -> RTree a -> RTree a
prune 0 (R x _) = R x []
prune _ (R x []) = (R x [])
prune n (R x lrt) = R x (map (prune (n-1)) lrt)

--d

mirror :: RTree a -> RTree a
mirror (R x lrt) = R x (map mirror lrt)

--e

postorder :: RTree a -> [a]
postorder (R x lrt)= 
    (concat (map postorder lrt)) ++ [x]

---Exercicio 1

data ExpInt = Const Int
               |Simetrico ExpInt
               |Mais ExpInt ExpInt
               |Menos ExpInt ExpInt
               |Mult ExpInt ExpInt

e1 = Mult (Mais (Simetrico (Const 5)) (Const 3)) (Simetrico (Const 7))


calcula :: ExpInt -> Int
calcula (Const x)= x 
calcula (Simetrico e)= -(calcula e)
calcula (Mais e1 e2)= calcula e1 + calcula e2
calcula (Menos e1 e2)= calcula e1 - calcula e2
calcula (Mult e1 e2)= calcula e1 * calcula e2

infixa :: ExpInt -> String
infixa (Const x)= show x 
infixa (Simetrico e)= "-" ++ infixa e 
infixa (Mais e1 e2)= "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2)= "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2)= "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x)= show x 
posfixa (Simetrico e)= posfixa e ++ "-"  
posfixa (Mais e1 e2)= posfixa e1 ++  " " ++ posfixa e2 ++ "+"
posfixa (Menos e1 e2)= posfixa e1 ++  " " ++ posfixa e2 ++ "-"
posfixa (Mult e1 e2)= posfixa e1 ++  " " ++ posfixa e2 ++ "*"



data Frac = F Integer Integer

mdc :: Integer -> Integer -> Integer
mdc x y
  | x==y = x
  | x<y = mdc x (y-x)
  | x>y = mdc (x-y) y

normaliza :: Frac -> Frac
normaliza (F n d) =
    let s = if n*d>0 then -1 else 1
        k = mdc (abs n) (abs d)
    in F (s*(div n k)) (div d k)

instance Eq Frac where
    (==) (F n1 d1) (F n2 d2) = n1*d2 == n2*d1

instance Show Frac where
    show (F 0 d) = show 0
    show (F n 1) = show n
    show (F n d) = show n ++"/"++ show d

instance Ord Frac where
    compare (F n1 d1) (F n2 d2)
      | n1*d2 < n2*d1 = LT 
      | n1*d2 == n2*d1 = EQ  
      | n1*d2 > n2*d1 = GT 

instance Num Frac where
    (+) (F n1 d1) (F n2 d2) = normaliza (F (n1*d2+n2*d1) (d1*d2))
    (-) (F n1 d1) (F n2 d2) = normaliza (F (n1*n2) (d1*d2))
    (*) (F n1 d1) (F n2 d2) = normaliza (F (n1*d2-n2*d1) (d1*d2))
    negate (F n d) = normaliza (F (-n) d)
    abs (F n d) = normaliza (F (abs n) (abs d))
    signum (F n d) = F (signum (n*d)) 1
    fromInteger x = F x 1 

soDobros :: Frac -> [Frac] -> [Frac]
soDobros f lf = filter (>(2*f)) lf




--data ExpInt = Const Int
--               |Simetrico ExpInt
--               |Mais ExpInt ExpInt
--               |Menos ExpInt ExpInt
--               |Mult ExpInt ExpInt
--
--e1 = Mult (Mais (Simetrico (Const 5)) (Const 3)) (Simetrico (Const 7))


--calcula :: ExpInt -> Int
--calcula (Const x)= x 
--calcula (Simetrico e)= -(calcula e)
--calcula (Mais e1 e2)= calcula e1 + calcula e2
--calcula (Menos e1 e2)= calcula e1 - calcula e2
--calcula (Mult e1 e2)= calcula e1 * calcula e2
--
--infixa :: ExpInt -> String
--infixa (Const x)= show x 
--infixa (Simetrico e)= "-" ++ infixa e 
--infixa (Mais e1 e2)= "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
--infixa (Menos e1 e2)= "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
--infixa (Mult e1 e2)= "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"
--
--posfixa :: ExpInt -> String
--posfixa (Const x)= show x 
--posfixa (Simetrico e)= posfixa e ++ "-"  
--posfixa (Mais e1 e2)= posfixa e1 ++  " " ++ posfixa e2 ++ "+"
--posfixa (Menos e1 e2)= posfixa e1 ++  " " ++ posfixa e2 ++ "-"
--posfixa (Mult e1 e2)= posfixa e1 ++  " " ++ posfixa e2 ++ "*"

--data Exp a = Const a
--               |Simetrico (Exp a)
--               |Mais (Exp a) (Exp a)
--               |Menos (Exp a) (Exp a)
--               |Mult (Exp a) (Exp a)
--
--calcula :: Num a => Exp a -> Int
--calcula (Const x)= x 
--calcula (Simetrico e)= -(calcula e)
--calcula (Mais e1 e2)= calcula e1 + calcula e2
--calcula (Menos e1 e2)= calcula e1 - calcula e2
--calcula (Mult e1 e2)= calcula e1 * calcula e2
--
--infixa :: Eq a => Exp a -> String
--infixa (Const x)= show x 
--infixa (Simetrico e)= "-" ++ infixa e 
--infixa (Mais e1 e2)= "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
--infixa (Menos e1 e2)= "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
--infixa (Mult e1 e2)= "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"
--
--instance Show (Exp a) where
--    show e = infixa e
--
--instance (Eq a,Num a) => Eq (Exp a) where
--    (==) e1 e2 = (calcula e1) == (calcula e2)
--
--instance Num a => Num (Exp a) where
--    (+) e1 e2 = Const (calcula e1 + calcula e2)
--    (-) e1 e2 = Const (calcula e1 - calcula e2)
--    (*) e1 e2 = Const (calcula e1 * calcula e2)
--    negate e = Const (negate (calcula e))
--    abs e = Const (abs (calcula e))
--    signum e = Const (signum (calcula e))
--    fromInteger x = Const (fromInteger x)



data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]



instance Eq Data where
    (==) (D d1 m1 a1) (D d2 m2 a2) = d1==d2 && m1==m2 && a1==a2

instance Ord Data where
    compare (D d1 m1 a1) (D d2 m2 a2) 
        | (a1,m1,d1) < (a2,m2,d2) = LT
        | (a1,m1,d1) == (a2,m2,d2) = EQ
        | (a1,m1,d1) > (a2,m2,d2) = GT

instance Show Data where
    show (D d m a) = show a ++ "/" ++ show m ++ "/" ++ show d

--ordena :: Extracto -> Extracto
--ordena (Ext si lm) =
--    Ext si (sortOn (\(d,s,m)-> d) lm)
--
--
--saldo :: Extracto -> Extracto
--saldo (Ext si lm) = let (sc,sd) = somaCredDeb lm 
--                    in sc+si-sd
--        where somaCredDeb :: [(Data,String,Movimento)] -> (Float,Float)
--              somaCredDeb [] = (0,0)
--              somaCredDeb ((_,_,m):lm) =
--                let (sc,sd) = somaCredDeb lm
--                in case m of
--                    Credito x -> (sc+x,sd)
--                    Debito x -> (sc,sd+x)
--
--col :: String -> Int -> String
--col s n = take n (s++ replicate n ' ')
--
--
--instance Show Extracto where 
--    show (Ext si lm) =
--        "Saldo anterior: " ++ show si ++ "\n" ++
--        replicate (4*12) '-' ++ "\n" ++
--        col "Data" 12 ++ col "Descricao" 12 ++ col "Credito" 12 ++ col "Debito" 12 ++ "\n" ++
--        replicate (4*12) '-' ++ "\n" ++
--        listaMovimentos lm ++ "\n" ++
--        replicate (4*12) '-' ++ "\n" ++
--        "Saldo actual:" ++ show (saldo (Ext si lm))
--
--
--listaMovimentos :: [(Data,String,Movimento)]-> String
--listaMovimentos [] = ""
--listaMovimentos ((d,desc,Credito x):lm)= 
--   col (show d) 12 ++ col desc 12 ++ col (show x) 12 ++ "\n" ++ 
--   listaMovimentos lm
--listaMovimentos ((d,desc,Debito x):lm)= 
--   col (show d) 12 ++ col desc 12 ++ replicate 12 " " ++col (show x) 12 ++ "\n" ++ 
--   listaMovimentos lm--


--b)

mastermind :: IO ()
mastermind = do ln <- geraNumeros;
                joga ln

joga :: [Int] -> IO ()
joga ln = do lnj <- leJogo;
             if ln==lnj then putStr "Ganhou!"
              else 
                do let lnc= concat(zipWith (\x y-> if x==y then [x] else []) ln lnj);
                   let ln' = ln\\lnc;
                   let lnj' = lnj\\lnc;
                   let lnpe = comuns ln' lnj';
                   putStrLn $ "Certos nas posições certas: " ++ (show $ length lnc);
                   putStrLn $ "Certos nas posições erradas: " ++ (show $ length lnpe);
                   joga ln


comuns :: [Int] -> [Int] -> [Int]
comuns [] l = []
comuns (x:xs) ys 
  | elem x ys = x : comuns xs (ys\\[x])
  | otherwise= comuns xs ys


geraNumeros :: IO [Int]
geraNumeros = do n1 <- randomRIO (0,9);
                 n2 <- randomRIO (0,9);
                 n3 <- randomRIO (0,9);
                 n4 <- randomRIO (0,9);
                 return [n1,n2,n3,n4]

leJogo :: IO [Int]
leJogo =
    do putStrLn "Introduza 4 digitos separados por espaços e termine com ENTER"
       s<- getLine;
       let lnj = map (digitToInt.head) (words s);
       return lnj


--Exercicio 2

data Aposta = Ap [Int] (Int,Int)

valida :: Aposta -> Bool
valida (Ap l (x,y)) = (length l ==5) && (all (\x -> elem x [1..50]) l) && (nub l)==l && 
                      all (\x -> elem x [1..9]) [x,y] && x/=y

comuns' :: Aposta -> Aposta -> (Int,Int)
comuns' (Ap ln (x,y)) (Ap ln1 (z,w))=
     (length (intersect ln ln1), length (intersect [x,y] [z,w]))

instance Eq Aposta where
    (==) ap1 ap2 = (comuns' ap1 ap2) == (5,2)

premio :: Aposta -> Aposta -> Maybe Int
premio ap1 ap2 = case (comuns' ap1 ap2) of
                    (5,2) -> Just 1
                    (5,1) -> Just 2
                    (5,0) -> Just 3
                    (4,2) -> Just 4
                    (4,1) -> Just 5
                    (4,0) -> Just 6
                    (3,2) -> Just 7
                    (2,2) -> Just 8
                    (3,1) -> Just 9
                    (3,0) -> Just 10
                    (1,2) -> Just 11
                    (2,1) -> Just 12
                    (2,0) -> Just 13
                    _     -> Nothing

geraNumeros' :: [Int] -> Int -> (Int,Int) -> IO [Int]
geraNumeros' l n (i,s) = do if (length l) == n then return l
                             else do k <- randomRIO (i,s);
                                     if elem k l then geraNumeros' l n (i,s)
                                                 else geraNumeros' (k:l) n (i,s)

geraChave :: IO Aposta
geraChave = do ln <- geraNumeros' [] 5 (1,50);
               [x,y] <- geraNumeros' [] 2 (1,9);
               return (Ap ln (x,y))

leAposta :: IO Aposta
leAposta = do putStrLn "Insira 5 números entre 1 e 50, separados por espaços, e termine com ENTER: "
              s  <- getLine;
              let ln = map read (words s) 
              putStrLn "Insira 2 números entre 1 e 9, separados por espaços, e termine com ENTER: "
              s1 <- getLine;
              let [x,y] = map read (words s1)
              let ap = Ap ln (x,y)
              if valida ap then return ap 
                           else do putStrLn "Aposta inválida";
                                   leAposta;

joga' :: Aposta -> IO ()
joga' ap = do apj <- leAposta;
            let p= premio ap apj;
            putStrLn (mostraPremio p)

mostraPremio :: Maybe Int -> String
mostraPremio Nothing = "Sem prémio!"
mostraPremio (Just p) = "Tem o prémio: " ++ show p

ciclo :: Aposta -> IO ()
ciclo ap = do op <- menu;
                case op of
                    "1" -> do {joga' ap; ciclo ap}
                    "2" -> do {ch <- geraChave; ciclo ch}
                    "0" -> putStrLn "Fim"
                    _ -> ciclo ap