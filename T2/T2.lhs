> -- Trabalho desenvolvido e criado por:
> -- Nome: Lívia Pereira Ozório 
> -- Matrícula: 201835011
> -- Nome: Pedro Henrique Filgueiras dos Santos Oliveira
> -- Matrícula: 201835024

> import System.Random
> import Data.List

> -- Cria estrutura Jogador. 
> data Jogador = Jogador String [[Int]] [[Bool]] String deriving Show

> -- Função para pegar o nome do jogador --
> nome (Jogador n _ _ _) = n

> -- Função para pegar a cartela do jogador
> cartela (Jogador _ c _ _) = c

> -- Função para pegar modo do jogador (linha ou coluna)
> modo (Jogador _ _ _ m) = m

> -- Função para pegar cartela bolleana (Cartela que indica se saiu ou não)
> cartela_bool (Jogador _ _ b _) = b

> -- Cria cartela bolleana dos jogadores
> cria_cartela_bool = do
>   let x = []
>   let y = [False,False,False,False,False]
>   let x1 = y:x
>   let x2 = y:x1
>   let x3 = y:x2
>   let x4 = y:x3
>   let x5 = y:x4
>   x5

> -- Cria cartela dos jogadores
> cria_cartela x l = do
>   if x == 1 then do
>       a <- (randomLinha x)
>       let aux = a:l
>       return aux
>   else do
>       a <- (randomLinha x)
>       let aux = a:l
>       cria_cartela (x-1) aux

> -- Confere se modo de jogo inserido é valido
> confere_modo nome modo = do
>   if modo == "l" || modo == "c" then do
>       return modo
>   else do
>       putStrLn "======================= ERRO ======================"
>       putStrLn "O modo de jogo deve ser (l) ou (c)"
>       modo2 <- insere_modo nome
>       confere_modo nome modo2

> -- insere o modo de jogo
> insere_modo nome = do
>   putStrLn ("Digite o tipo da cartela de " ++ nome ++ " −− (l) para linha e (c) para coluna :")  
>   let modo = getLine
>   modo

> -- Cria jogadores
> inicia_jogador n x j = do
>   if n == 1 then do
>       putStrLn ("Qual o nome do jogador"  ++ show x ++ "?")
>       nome <- getLine
>       modo <- insere_modo nome
>       modo <- confere_modo nome modo
>       cartela <- cria_cartela 5 []
>       let cartela_bool = cria_cartela_bool
>       let jog = Jogador nome cartela cartela_bool modo
>       let a = jog:j
>       return a
>   else do
>       putStrLn ("Qual o nome do jogador"  ++ show x ++ "?") 
>       nome <- getLine
>       modo <- insere_modo nome
>       modo <- confere_modo nome modo
>       cartela <- cria_cartela 5 []
>       let cartela_bool = cria_cartela_bool
>       let jog = Jogador nome cartela cartela_bool modo
>       let a = jog:j
>       inicia_jogador (n-1) (x+1) a

> -- Sorteia numero randomico
> sorteiaInt :: Int -> Int -> IO Int
> sorteiaInt min max = getStdRandom (randomR (min,max))

> -- Sorteia linha randomica da cartela
> randomLinha :: Int -> IO([Int])
> randomLinha n = do
>   let min = 1 + (n-1) * 15
>   let max = min + 14
>   g <- newStdGen
>   let l = sort $ take 5 . nub $ (randomRs (min,max) g :: [Int])
>   return l

> -- Acha se um elemento contem na lista
> recFind :: [Int] -> Int -> Int -> Int
> recFind [] _ _ = -1
> recFind (x:xs) n i = 
>    if x == n then
>        i
>    else
>        recFind xs n (i+1)

> -- Busca elemento na lista
> buscaElemento :: [Int] -> Int -> Int
> buscaElemento l n = recFind l n 0

> -- Imprime linha da cartela
> imprime_linha :: [Int] -> [Bool] -> IO ()
> imprime_linha (v:vs) (b:bs) = do
>   if vs == [] then do
>       if b == False then do
>           putStr ((show v) ++ "[ ] " )
>       else do
>           putStr ((show v) ++ "[X] " )
>   else do
>       if b == False then do
>           putStr ((show v) ++ "[ ] " )
>           imprime_linha vs bs
>       else do
>           putStr ((show v) ++ "[X] " )
>           imprime_linha vs bs

> -- Imprime a cartela 
> imprime_cartela :: [[Int]] -> [[Bool]] -> IO ()
> imprime_cartela (c:cs) (b:bs) = do
>   if cs == [] then do
>       imprime_linha c b
>       putStrLn ("")
>   else do
>       imprime_linha c b
>       putStrLn ("")
>       imprime_cartela cs bs

> -- Imprime os dados do jogador
> imprime_jogador (j:js) = do
>   if length js == 0 then do
>       if (modo j) == "l" then do
>           putStrLn ("==================== " ++ (nome j) ++ ": linha =====================")
>       else do
>           putStrLn ("=================== " ++ (nome j) ++ ": coluna =====================")
>       imprime_cartela (cartela j) (cartela_bool j)
>   else do 
>       if (modo j) == "l" then do
>           putStrLn ("==================== " ++ (nome j) ++ ": linha =====================")
>       else do
>           putStrLn ("==================== " ++ (nome j) ++ ": coluna ====================")
>       imprime_cartela (cartela j) (cartela_bool j)
>       imprime_jogador js

> -- Imprime dados
> imprime_inicio :: [Jogador] -> IO ()
> imprime_inicio js = do
>   putStrLn "==================================================="
>   putStrLn "Cartelas"
>   putStrLn "==================================================="
>   imprime_jogador js

> -- Imprime dados
> imprime_meio :: [Jogador] -> IO ()
> imprime_meio js = do
>   putStrLn "Cartelas"
>   imprime_jogador js

> -- Imprime vencedor
> imprime_parabens :: [Jogador] -> [Bool] -> IO()
> imprime_parabens (j:js) (b:bs) = do
>   if bs == [] then do
>       if b then do
>           putStrLn ("Parabens " ++ (nome j) ++ "!")
>       else do
>           putStr ""
>   else 
>       if b then do
>           putStrLn ("Parabens " ++ (nome j) ++ "!")
>           imprime_parabens js bs
>       else do
>           putStr ""
>           imprime_parabens js bs

> -- Procura uma determinada linha da cartela
> procura_linha :: [[Int]] -> Int -> [Int]
> procura_linha (l:ls) n = do
>   if n == 1 then do
>       l
>   else do
>       procura_linha ls (n-1)

> -- Atualiza linhas de bolleanos
> update_bool :: [Bool] -> Int -> [Bool]
> update_bool (b:bs) n = do
>   if n == 0 then do
>       True:bs
>   else do
>       b:(update_bool bs (n-1))

> -- Atualiza cartela de bolleanos
> update_cartela_bool :: [[Bool]] -> Int -> Int -> [[Bool]]
> update_cartela_bool (b:bs) n p = do
>   if p == 1 then do
>       (update_bool b n):bs
>   else do
>       b:(update_cartela_bool bs n (p-1))

> -- Atualiza jogadore
> aux_update_jogador :: Jogador -> Int -> Jogador
> aux_update_jogador j n = do
>   let c = cartela j
>   let b = cartela_bool j
>   if n >= 1 && n <=15 then do
>       let linha = procura_linha c 1
>       let e = buscaElemento linha n
>       if e /= -1 then do
>           let b_new = update_cartela_bool b e 1
>           Jogador (nome j) c b_new (modo j)
>       else do
>           j
>   else do
>       if n >= 16 && n <=30 then do
>           let linha = procura_linha c 2
>           let e = buscaElemento linha n
>           if e /= -1 then do
>               let b_new = update_cartela_bool b e 2
>               Jogador (nome j) c b_new (modo j)
>           else do
>               j           
>       else do
>           if n >= 31 && n <=45 then do
>               let linha = procura_linha c 3
>               let e = buscaElemento linha n
>               if e /= -1 then do
>                   let b_new = update_cartela_bool b e 3
>                   Jogador (nome j) c b_new (modo j)
>               else do
>                   j   
>           else do
>               if n >= 46 && n <=60 then do
>                   let linha = procura_linha c 4
>                   let e = buscaElemento linha n
>                   if e /= -1 then do
>                       let b_new = update_cartela_bool b e 4
>                       Jogador (nome j) c b_new (modo j)
>                   else do
>                       j    
>               else do
>                   let linha = procura_linha c 5
>                   let e = buscaElemento linha n
>                   if e /= -1 then do
>                       let b_new = update_cartela_bool b e 5
>                       Jogador (nome j) c b_new (modo j)
>                   else do
>                       j 

> -- Atualiza lista de jogadores
> update_jogador :: [Jogador] -> Int -> [Jogador]
> update_jogador (j:js) n = do
>   if length js == 0 then do
>       (aux_update_jogador j n):js   
>   else do
>       (aux_update_jogador j n):(update_jogador js n)

> -- Confere os valores das colunas
> confere_coluna_aux :: Int -> [Bool] -> Bool
> confere_coluna_aux n (b:bs) = do
>   if n == 1 then do
>       b
>   else do
>       confere_coluna_aux (n-1) bs 

> -- Confere as colunas
> confere_coluna :: Int -> Int -> [[Bool]] -> [Bool]
> confere_coluna x n (b:bs) = do
>   if x == 5 then do
>       (confere_coluna_aux n b):[]
>   else do
>       (confere_coluna_aux n b):(confere_coluna (x+1) n bs)

> -- Recursividade para passar pelas 5 colunas
> confere_coluna_5 :: Int -> [[Bool]] -> Bool
> confere_coluna_5 n b = do
>   if n == 5 then do
>       if (and (confere_coluna 1 n b)) then do
>           True
>       else do
>           False
>   else do 
>       if (and (confere_coluna 1 n b)) then do
>           True
>       else do
>           confere_coluna_5 (n+1) b

> -- Confere as linhas
> confere_linha :: [[Bool]] -> Bool
> confere_linha (b:bs) = do
>   if bs == [] then do
>       if (and  b) then do
>           True
>       else do
>           False
>   else 
>       if (and  b) then do
>           True
>       else do
>           confere_linha bs  

> -- Percorre os jogadores avaliando se venceram
> percorre_jogador :: [Jogador] -> [Bool] -> [Bool]
> percorre_jogador (j:js) b = do
>   if length js == 0 then do
>       if (modo j) == "l" then do 
>           (confere_linha (cartela_bool j)):b
>       else do
>           (confere_coluna_5 1 (cartela_bool j)):b
>   else do
>       if (modo j) == "l" then do 
>           (confere_linha (cartela_bool j)):(percorre_jogador js b)
>       else do
>           (confere_coluna_5 1 (cartela_bool j)):(percorre_jogador js b)

> -- Realiza as jogadas 
> jogada :: [Jogador] -> [Int] -> IO ()
> jogada j l = do
>   n <- sorteiaInt 1 75
>   let e = buscaElemento l n
>   if e == -1 then do
>       let aux = n:l
>       putStr ("=============== Valor Sorteado: " ++ show n ++ " ================")
>       putStrLn ""
>       let list_new = update_jogador j n
>       let ganhou = percorre_jogador list_new []
>       if (or ganhou) then do
>           imprime_meio list_new
>           imprime_parabens list_new ganhou 
>       else do
>           imprime_meio list_new
>           jogada list_new aux
>   else do
>       jogada j l

> -- Função de inicio
> main = do 
>   putStrLn "Informe a quantidade de jogadores"
>   quant <- getLine
>   let q = (read quant :: Int)
>   if q >= 2 && q <=4 then do
>       list_jogador <- inicia_jogador q 1 []
>       imprime_inicio list_jogador
>       jogada list_jogador []
>   else do
>       putStrLn "======================= ERRO ======================"
>       putStrLn "O numero de jogadores deve varias de 2 a 4"
>       main

