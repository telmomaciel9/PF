module Ficha4 where 

import Data.Char
import Data.List

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