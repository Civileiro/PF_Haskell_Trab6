
formatExample :: [Char] -> [Char] -> [Char] -> [Char]
formatExample n arg result = "Func." ++ n ++ ":entrada:" ++ arg ++ ":resultado:" ++ result

showExample :: (Show a, Show b, Show c) => a -> [b] -> c -> [Char]
showExample n arg result = formatExample nS argS resultS
  where
  nS = [x | x <- show n, notElem x "\"\\"]
  argS = [x | x <- unwords (map show arg), notElem x "\"\\"]
  resultS = show result

main = do
  putStrLn $ showExample 1 [1729] (divisoresden 1729)
  putStrLn $ showExample 1 [-10] (divisoresden (-10))
  putStrLn $ showExample 2 ["'a'", "'abacate'"] (contaCaractere 'a' "abacate")
  putStrLn $ showExample 2 ["'x'", "'haskell'"] (contaCaractere 'x' "haskell")
  putStrLn $ showExample 3 [[1, -2, 3, -4, 5]] (dobroNaoNegativo [1, -2, 3, -4, 5])
  putStrLn $ showExample 3 [[-3, 5, -11, 15, -25]] (dobroNaoNegativo [-3, 5, -11, 15, -25])
  putStrLn $ showExample 4 [25] (pitagoras 25)
  putStrLn $ showExample 4 [50] (pitagoras 50)
  putStrLn $ showExample 5 [100] (numerosPerfeitos 100)
  putStrLn $ showExample 5 [500] (numerosPerfeitos 500)
  putStrLn $ showExample 6 [[1, 3, 5], [2, 4, 6]] (produtoEscalar [1, 3, 5] [2, 4, 6])
  putStrLn $ showExample 6 [[-3, 5], [0, 1, -8]] (produtoEscalar [-3, 5] [0, 1, -8])
  putStrLn $ showExample 7 [0] (primeirosPrimos 0)
  putStrLn $ showExample 7 [5] (primeirosPrimos 5)
  putStrLn $ showExample 7 [20] (primeirosPrimos 20)
  putStrLn $ showExample 8 [3] (paresOrdenados 3)
  putStrLn $ showExample 8 [10] (paresOrdenados 10)
  putStrLn $ showExample 8 [20] (paresOrdenados 20)


{-
1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado.
-}
divisoresden :: Int -> [Int]
divisoresden n = [x|x <- [1..abs n], n `mod` x == 0]

{-
2. Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte a ocorrência de um caractere específico, em uma string dada.
-}
contaCaractere :: Char -> String -> Int
contaCaractere c xs = length [x | x <- xs, x == c]

{-
3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada.
-}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo xs = [2*x | x <- xs, x >= 0]

{-
4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado. 
-}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras x = 
  [
  (a, b, c) | 
    m <- [1..x], 
    n <- [1..x], 
    m > n,
    let c = m^2 + n^2,
    c < x,
    let a = m^2 - n^2,
    let b = 2*m*n
  ]

{-
5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado.
-}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n  = [x | x <- [0..n], (sum.divisoresden) x == 2*x]

{-
6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis. 
-}
produtoEscalar :: [Int] -> [Int] -> [(Int, Int)]
produtoEscalar xs ys = [(x, y) | x <- xs, y <- ys]

{-
7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva uma lista contendo os n primeiros números primos a partir do número 2.
-}
naoDivide :: Integer -> Integer -> Bool
naoDivide x n = x `mod` n /= 0

primeirosPrimos :: Int -> [Integer]
primeirosPrimos n = take n [x | x <- [2..], all (naoDivide x) [2..x-1]]

{-
8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes.
-}
paresOrdenados :: Int -> [(Integer, Integer)]
paresOrdenados n = take n [(x^2, x^3) | x <- [0..]]