> import System.Random
> import Data.List

> data Jogador = Jogador String [[String]] deriving Show

> nome (Jogador n _ ) = n
> cartela (Jogador _ c) = c

> sorteiaInt :: Int -> Int -> IO Int
> sorteiaInt min max = getStdRandom (randomR (min,max))

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

> main = do 
>    putStrLn "Informe a quantidade de jogadores"
>    quant <- getLine
>    let q = (read quant :: Int)
>    inicia_jogador q 1

