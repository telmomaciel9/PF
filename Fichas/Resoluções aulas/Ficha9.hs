import System.Random 
import Data.Char
import Data.List
import Data.Maybe

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
                    