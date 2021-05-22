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