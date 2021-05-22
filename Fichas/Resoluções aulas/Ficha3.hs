



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