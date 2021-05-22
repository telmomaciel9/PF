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
