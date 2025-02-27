import Data.Map (Map)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Char (toUpper)
import Data.List (genericLength)
import Debug.Trace (trace)

-- 1 Funcion para aplicar Descuento

descuento :: Double -> Double -> Double
descuento precio porcentaje = precio - (precio * porcentaje / 100)

-- Funcion para aplicar IVA

iva :: Double -> Double -> Double
iva precio porcentajeIva = precio + (precio * porcentajeIva / 100)

-- Aplicar función a una cesta de la compra

lCesta :: Map String (Double, Double) -> (Double -> Double -> Double) -> Map String Double
lCesta cesta f = Map.map (\(precio, porcentaje) -> f precio porcentaje) cesta

-- 2. Aplicar función a cada elemento de una lista

fLista :: (a -> b) -> [a] -> [b]
fLista f lista = map f lista

-- 3. Contar palabras en una frase

palabras :: String -> Map String Int
palabras frase = Map.fromList [(palabra, length palabra) | palabra <- words frase]

-- 4. Convertir notas a calificaciones

calificaciones :: Map String Int -> Map String String
calificaciones notas = Map.map calificar notas
  where
    calificar n
      | n >= 95 = "Excelente"
      | n >= 85 = "Notable"
      | n >= 75 = "Bueno"
      | n >= 70 = "Suficiente"
      | otherwise = "Desempeno insuficiente"

-- 5. Cálculo del módulo de un vector

modulo :: [Double] -> Double
modulo v = sqrt $ sum [x^2 | x <- v]

-- 6. Encontrar valores atípicos

atipicos :: [Double] -> [Double]
atipicos xs 
    | length xs < 2 = [] 
    | otherwise =
        let 
            n = genericLength xs
            media = sum xs / n
            varianza = sum [(x - media) ^ 2 | x <- xs] / (n - 1) 
            desviacion = sqrt varianza
            zScores = [(x, (x - media) / desviacion) | x <- xs]
        in 
            trace ("Media: " ++ show media ++ ", Desviación: " ++ show desviacion ++ 
                   "\nPuntuaciones: " ++ show zScores) 
            [x | (x, z) <- zScores, abs z > 2] 



carrito :: IO ()
carrito = do

    let cesta = Map.fromList [("PC GAMER", (10000, 10)), ("MONITOR 4K", (5000, 5))]
    print $ lCesta cesta descuento 
    print $ lCesta cesta iva       

notas :: IO ()
notas = do
    let notas1 = Map.fromList [("WEB", 99), ("AUTOMATAS", 74), ("MATE DISCRETAS", 85)]
    let notas2 = Map.fromList [("REDES", 90), ("SISTEMAS", 60), ("ADMON DB", 89)]
    let notas3 = Map.fromList [("PRECALCULO", 0), ("ETICA", 72), ("LOGICA", 85)]
    print $ calificaciones notas1 
    print $ calificaciones notas2 
    print $ calificaciones notas3 

main :: IO ()
main = do
    print $ atipicos [10, 12, 14, 15, 100, 101] -- Esperado: [100, 101]
    print $ atipicos [50, 51, 52, 1000] -- Esperado: [1000]
    print $ atipicos [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] -- Esperado: []
