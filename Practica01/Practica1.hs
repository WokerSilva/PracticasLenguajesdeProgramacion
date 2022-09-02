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
-- Uso: < (calculadora "div" (20, 3) >  
-- Uso: < (calculadora "sum" (20, 3) >  
calculadora :: String -> (Int ,Int) -> Int
calculadora "first" (x,_) = x       -- Devuelve el primer elemento de la tupla
calculadora "second" (_,y) = y      -- Devuelve el segundo elemento de la tupla
calculadora "sum" (x,y) = x + y     -- Devuelve la suma de la tupla
calculadora "dif" (x,y) = x - y     -- Devuelve la resta de la tupla
calculadora "mul" (x,y) = x * y     -- Devuelve la multiplicación de la tupla
calculadora "div" (x,y) = div x y   -- Devuelve la división de la tupla
calculadora "pow" (x,y) = x^(y)     -- Devuelve la potencia de la tupla

loki :: Int -> Bool -> String
loki = error "Definir el cuerpo de la función"

numeroFormas :: Int -> Int
numeroFormas = error "Definir el cuerpo de la función"

divisoresPropios :: Int -> [Int]
divisoresPropios = error "Definir el cuerpo de la función"

cribaEratostenes :: Int -> [Int]
cribaEratostenes = error "Definir el cuerpo de la función"

aproxima :: Float -> Float
aproxima = error "Definir el cuerpo de la función"

perfectos :: [Int] -> [Int]
perfectos = error "Definir el cuerpo de la función"