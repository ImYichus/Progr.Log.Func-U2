
-- Suma de elementos en una lista
-- Estoy utilizando la funcion SUM para resolver este ejercicio
sumar :: [Int] -> Int
sumar = sum
-- Factorial
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)
-- Números pares
-- Aqui lo que hago es decir que se filtren los pares hasta n
pares :: Int -> [Int]
pares n = [x | x <- [0..n], even x]
-- Longitud de una cadena
-- Aqui utilizo la funcion length 
lengCadena :: String -> Int
lengCadena = length
-- Reverso de una lista
-- Aqui utilizo una funcion nuevo que es reverse que lo que hace es poner al reves los elementos de una lista
reverso :: [a] -> [a]
reverso = reverse
-- Duplicar elementos
-- Aqui utilizo la funcion concatMap que lo que hace es duplicar los elemtos de una lista
duplicar :: [Int] -> [Int]
duplicar = concatMap (\x -> [x, x])
-- Filtrar elementos pares
-- Aqui utilizo el filter que por lo que entendi filtra los elementos si se cumple una cierta condicion
filtPares :: [Int] -> [Int]
filtPares = filter even
-- Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
-- Divisores de un número
-- Aqui lo que hice fue generar y filtrar una lista donde se cumpla la condicion de que son divisores de n
-- utilizo el operador mod
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]
-- Palíndromo
palindromo :: String -> Bool
palindromo s = s == reverse s
