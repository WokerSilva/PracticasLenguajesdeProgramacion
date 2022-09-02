-- Practica 01 
-- Lenguajes de Programación 
-- V1.0
-- Silva Huerta Marco 316205326

module Practica1 where 

-- Ejercicio 1
-- Función que devuelve el área lateral de un paralepípedo rectángulo 
-- Recibe: Largo, ancho y altura (Float)
-- Devuelve: Un valor de tipo Float
-- Uso: < areaLateral 3 6 8 >

areaLateral :: Float -> Float -> Float -> Float
areaLateral a b c = 2 * (a + b) * c


-- Ejercicio 2
-- Función que devuelve el área total de un cono circular recto 
-- Recibe: generatriz y diámetro del cono (Float)
-- Devuelve: Un valor de tipo Float
-- Uso: < areaTotal 4 7.6 > 

areaTotal :: Float -> Float -> Float
areaTotal r g = pi * r * g + pi * (r) ^ 2


-- Ejercicio 3
-- Función que devuelve la distancia entre dos puntos 
-- Recibe: dos tuplas de numeros Float
-- Devuelve: Una tupla de tipo Float
-- Uso: < distancia (1.3,4) (3,4.5) >  

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)


-- Ejercicio 4
-- Función que devuelve la implicación logica de dos valores booleanos
-- Recibe: dos booleanos
-- Devuelve: un booleano resultado de la implicación
-- Uso: < impl True False >  

impl :: Bool -> Bool -> Bool
impl True False = False
impl x y = True


-- Ejercicio 5
-- Función que al recibir un parametro de tipo String 
--     y dos parametros de tipo entero ejecuta una operación 
-- Recibe: Un String y Dos Enteros
-- Devuelve: Un valor de tipo entero
-- Uso: < calculadora "div" (20, 3) >  
-- Uso: < calculadora "sum" (20, 3) >  

calculadora :: String -> (Int ,Int) -> Int
calculadora "first" (x,_) = x       -- Devuelve el primer elemento de la tupla
calculadora "second" (_,y) = y      -- Devuelve el segundo elemento de la tupla
calculadora "sum" (x,y) = x + y     -- Devuelve la suma de la tupla
calculadora "dif" (x,y) = x - y     -- Devuelve la resta de la tupla
calculadora "mul" (x,y) = x * y     -- Devuelve la multiplicación de la tupla
calculadora "div" (x,y) = div x y   -- Devuelve la división de la tupla
calculadora "pow" (x,y) = x^(y)     -- Devuelve la potencia de la tupla


-- Ejercicio 6
loki :: Int -> Bool -> String
loki = error "Definir el cuerpo de la función"

-- Ejercicio 7
-- Funcion que calcula de cuantas formas un niño puede subir escaleras 
--     de 3 formas: Subiendo 1 escalon, 2 escalones o 3 escalones 
-- Recibe: Un entero que es el número de escaleras
-- Devuelve: Un entero que es el número de formas en que pude subirlas
-- Uso: < numeroFormas 10 >

numeroFormas :: Int -> Int
numeroFormas 0 = 0
numeroFormas 1 = 1
numeroFormas 2 = 2
numeroFormas 3 = 4
numeroFormas n = numeroFormas(n - 3) + numeroFormas(n - 2) + numeroFormas(n - 1)


-- Ejercicio 8 (a)
divisoresPropios :: Int -> [Int]
divisoresPropios x = [y | y <- [1..x-1] , x `mod` y == 0]

-- Ejercicio 8 (b)
-- Funcion que aplica el algoritmo de Criba de Eratóstenes
--    Se utiliza una función auxiliar 
-- Recibe: Un entero (que será el rango para la lista)
-- Devuelve: Una lista con los números primos menores al número dado
-- Uso: < cribaEratostenes 20 >

cribaEratostenes ::  Int -> [Int]
cribaEratostenes n = auxCE [x | x <- [2..n]] 0
-- Función Auxiliar
auxCE :: [Int] -> Int -> [Int]
auxCE lista n 
  | n == length lista - 1 = lista
  | otherwise = auxCE [x | x <- lista, (x `mod` lista!! n)/= 0||x == lista!!n] (n+1)


-- Ejercicio 9 (a)
-- Funcion que recibe una lista de numeros y devuelve una lista
--    solo con números perfectos
--    Tiene una función "auxiliar" y el uso de filter para hacer
--    recorrer la lista y verificar que números son perfectos
-- Recibe: Una lista de números
-- Devuelve: Una lista de números (solo números perfectos)
-- Uso: < perfectos [1,2,4,6,496,8128,90000] >

perfectos :: [Int] -> [Int]
perfectos s = filter esPerfecto s
  where esPerfecto :: Int -> Bool
        esPerfecto n = n == sum [i | i <- [1..n-1], n `mod` i == 0]



-- Ejercicio 9  (b)
aproxima :: Float -> Float
aproxima = error "Definir el cuerpo de la función"


-- Ejercicio 10 
-- Aquí estamos definiendo un dato llamdo figura 
--     cada figura tiene especificado el tipo 
--     de dato que va recibir, en este caso serán 
--     tipo Float. Los datos que reciben son para 
--     calcular el area de las figuras.
data Figura = Triangulo Float Float           | 
              Cuadrado Float                  |
              Rectangulo Float Float          |
              Rombo Float Float               |
              Paralelogramo Float Float Float |
              Circulo Float                   |
              Elipse Float Float
-- Ejercicio 11
-- Funcion que calcula el área de figuras ya definidas
-- Recibe: Cada figura recibe los datos (Float) que necesita para calcular su area
-- Devuelve: Un valor Float que es su area
-- Uso: < area (Triangulo 34 65) >
-- Uso: < area (Cuadrado 3) >
-- Uso: < area (Rombo 15 9) >
area :: Figura -> Float
area (Triangulo b h) = (b*h)/2
area (Cuadrado l) = l^2
area (Rectangulo a b) = a*b
area (Rombo dm d) = (dm*d)/2
area (Paralelogramo a b h) = b*h
area (Elipse a b) = pi*a*b

main = do
  putStrLn "Hello"
  --putStrLn "World"
  print (impl True False)
  print (calculadora "div" (20, 3))
  print (numeroFormas 4)
  print (divisoresPropios 45)
  print (cribaEratostenes 20)
  print (perfectos [1,2,4,6,496,8128,90000])