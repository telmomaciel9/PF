module Exame where


type TabAbrev = [(Abreviatura,Palavra)]
type Abreviatura = String
type Palavra = String

tabela :: TabAbrev
tabela = [("sol","solzinho"),("pdf","powerpoint"),("mar","marinheiro")]

consulta :: TabAbrev -> Palavra -> Maybe Abreviatura
consulta [] pal = Nothing
consulta ((h,t):xs) pal 
   | t==pal = Just h
   | otherwise= consulta xs pal 

bd :: [Video]
bd = [Filme "missao impossivel" 2002,Filme "missao impossivel 2" 2005,Serie "adidas" 1 1]
bd2 = [Filme "missao impossivel" 2002,Filme "sojfsdjkl 3" 2005,Serie "nike" 1 1]

type BD = [Video]

data Video = Filme String Int       -- título, ano
           | Serie String Int Int   -- título, temporada, episódio
           | Show  String Int       -- título, ano
           | Outro String
   deriving (Eq,Show)

--que recebe duas bases de dados de vídeos  e faz a sua fusão eliminando repetidos.
fusao :: BD -> BD -> BD
fusao [] l = l
fusao (h:t) l
  | elem h l == True = fusao t l
  | otherwise = h: fusao t l

type Mat a = [[a]]

--que recebe um inteiro n e devolve uma matriz quadrada, de tamanho n, 
--com colunas alternadamente preenchidas só com 1s e só com 0s. 
--A primeira coluna é de 1s. Por exemplo, fun 3 == [[1,0,1],[1,0,1],[1,0,1]] 

fun :: Int -> Mat Int
fun 0 = []
fun n = replicate n (auxFun n)

auxFun :: Int -> [Int]
auxFun 0 = []
auxFun n 
  | even n = (auxFun (n-1))++ [0]
  | otherwise = (auxFun (n-1))++ [1]


data Pokedex = Empty | Pokemons Pokemon Pokedex Pokedex
type Pokemon = (Nome, Stamina, Tipo)
data Tipo = Agua | Fogo | Terra | Ar
   deriving Show
type Nome = String 
type Stamina = Int 

 



--que dado um inteiro, determina a lista de todos os nomes de pokémons e a sua stamina, 
--sempre que a stamina seja menor que dito inteiro. 
--A lista deve estar ordenada de forma decrescente pela stamina.

--pokemons 92 p1 deverá retornar [(“Psyduck”,90),(“Charmander”,55)]


pokemons :: Int -> Pokedex -> [(Nome,Int)]
pokemons n l = removeTipo (ordenaPokemon (procuraPokemon n l))

procuraPokemon :: Int -> Pokedex -> [Pokemon]
procuraPokemon n Empty = []
procuraPokemon n (Pokemons pok e d)
  | n> getStamina pok = [pok] ++ procuraPokemon n e ++ procuraPokemon n d
  | otherwise = procuraPokemon n e ++ procuraPokemon n d

getStamina :: Pokemon -> Int
getStamina (nome,stamina,tipo)= stamina

ordenaPokemon :: [Pokemon] -> [Pokemon]
ordenaPokemon [x]= [x]
ordenaPokemon (h:t) = insere h (ordenaPokemon t)

insere :: Pokemon -> [Pokemon] -> [Pokemon]
insere h [] = [h]
insere h (x:xs)
  | getStamina h > getStamina x = (h:x:xs)
  | otherwise = x : insere h xs

removeTipo :: [Pokemon] -> [(Nome,Int)]
removeTipo [] = []
removeTipo ((no,sta,ti):xs)= (no,sta): removeTipo xs

p1 = Pokemons ("Psyduck",90,Agua) 
    (Pokemons ("Charmander",55,Fogo) Empty Empty) --ramo esquerda
    (Pokemons ("Blastoid",95,Agua) Empty Empty) --ramo direita


type Data = (Int,Int,Int)
type TempMin = Int
type TempMax = Int
type Registo = (Data , TempMin, TempMax)
data Temperaturas = Vazia 
                  | Nodo Registo Temperaturas Temperaturas
                deriving Show

registo= Nodo ((25,1,2002),10,10)
         (Nodo ((10,1,2002),15,25) Vazia Vazia)
         (Nodo ((30,12,2002),10,20) Vazia Vazia)

--que calcula a temperatura mais alta registada e o dia em que ela ocorreu.
mTemp :: Temperaturas -> (TempMax,Data)
mTemp temps = (f, getDay temps f)
   where f= (getMaximun (getTemp temps))

getMaximun :: [Int] -> Int
getMaximun [x] = x
getMaximun (h:h2:t)
  | h > h2 = getMaximun (h:t)
  | otherwise = getMaximun (h2:t)

getTemp :: Temperaturas -> [Int]
getTemp Vazia = []
getTemp (Nodo (reg) e d) = [getTempMax reg] ++ getTemp e ++ getTemp d

getTempMax :: Registo -> Int
getTempMax (dat , min, max)= max

getData :: Registo -> Data
getData (dat , min, max)= dat

getDay :: Temperaturas -> Int -> Data
getDay (Nodo (reg) e d) n 
  | getTempMax reg==n = getData reg
  | elem n (getTemp e) = getDay e n
  | otherwise = getDay d n


func :: [[a]] -> Int
func l = length (filter (not . null) l)

func' :: Eq a => [[a]] -> Int
func' [[]] = 0
func' [[x]] = 1
func' (h:t)
  | h/=[] = 1 + func' t
  | otherwise = func' t


data BTree a = Empty
             | Node a (BTree a) (BTree a)
    deriving Show

bt = Node (fromIntegral 10)
     (Node (fromIntegral 7) Empty Empty)
     (Node (fromIntegral 15) (Node (fromIntegral 12) Empty Empty) Empty)

-- que, dada uma árvore de procura de inteiros, 
--pede ao utilizador para escrever um inteiro e escreve no écran, 
--por ordem crescente e separados por um espaço, 
--todos os elementos da árvore maiores que o número lido.

mostra :: BTree Int -> IO ()
mostra arvore = do 
                putStrLn "Insira um número inteiro:"
                x <- getLine
                let num = (read x) :: Int
                    lista = ordenaLista (getAllNum arvore)
                    maiores = getBig num lista                
                print maiores

getAllNum :: BTree Int -> [Int]
getAllNum Empty = []
getAllNum (Node x e d) = [x] ++ getAllNum e ++ getAllNum d

ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (h:t) = insere h (ordenaLista t)

insere :: Int -> [Int] -> [Int]
insere h [] = [h]
insere h (x:xs)
  | h < x = (h:x:xs)
  | otherwise = x : insere h xs

getBig :: Int -> [Int] -> [Int]
getBig n [] = []
getBig n (h:t) 
   | n<h = (h:t)
   | otherwise = getBig n t


data RTree a = R a [RTree a]
type Dictionary = [ RTree (Char, Maybe String) ]

d1 = [R ('c',Nothing) [
        R ('a',Nothing) [
            R ('r',Nothing) [
                R ('a',Just "...") [
                    R ('s',Just "...") []
                    ],
                R ('o',Just "...") [],
                R ('r',Nothing) [
                    R ('o',Just "...") []
                    ]
                ]
            ]
        ]
    ]


consulta :: String -> Dictionary -> Maybe String
consulta [x] [(R ('a',b) [])]= b
--consulta (h:t) (RTree R a l)
   