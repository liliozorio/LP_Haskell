> import System.Random
> import Data.List

> data Jogador = Jogador String [[String]] deriving Show

> nome (Jogador n _ ) = n
> cartela (Jogador _ c) = c

> sorteiaInt :: Int -> Int -> IO Int
> sorteiaInt min max = getStdRandom (randomR (min,max))

> randomLinha :: Int -> IO([Int])
> randomLinha n = do
>   let min = 1 + (n-1) * 15
>   let max = min + 14
>   g <- newStdGen
>   let l = sort $ take 5 . nub $ (randomRs (min,max) g :: [Int])
>   return l
 
> inicia_jogador :: Int-> Int -> IO () 
> inicia_jogador n x = do
>   if n == 1 then do
>       putStrLn ("Qual o nome do jogador"  ++ show x ++ "?")
>       nome <- getLine
>       putStrLn (nome)
>   else do
>       putStrLn ("Qual o nome do jogador"  ++ show x ++ "?") 
>       nome <- getLine
>       inicia_jogador (n-1) (x+1)

> recFind :: [Int] -> Int -> Int -> Int  --recursividade pra achar isso 
> recFind [] _ _ = -1
> recFind (x:xs) n i = 
>    if x == n then
>        i
>    else
>        recFind xs n (i+1)

> buscaElemento :: [Int] -> Int -> Int --retorna índice de valor N na lista L. Se não achar, retorna -1 
> buscaElemento l n = recFind l n 0
    

> funcaoTeste :: IO(Int) --Gera um numero aleatorio de 1 a 15 e uma linha de cartela e procura o indice
> funcaoTeste = do
>   x <- getStdRandom (randomR (1,15)) :: IO(Int)
>   print x
>   y <- randomLinha 1 :: IO([Int])
>   print y
>   return $ buscaElemento y x


> main = do 
>    putStrLn "Informe a quantidade de jogadores"
>    quant <- getLine
>    let q = (read quant :: Int)
>    inicia_jogador q 1

