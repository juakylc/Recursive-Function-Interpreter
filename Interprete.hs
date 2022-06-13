-- Interprete de funciones recursivas

{- Este fichero contiene el Interprete de funciones recursivas realizado
   por Joaquin Borrego Fernandez, alumno de Ciencias de la computacion
   del 4º curso del Grado en Matematicas de la Universidad de Sevilla

   En primer lugar, importamos el paquete Data.Maybe para usar el tipo
   de dato Maybe a, el cual esta formado por Nothing o Just a -}

import Data.Maybe

{- A continuacion, definiremos el resto de tipos de datos a usar, el
   cual sera Descr, formado por las funciones basicas Z,S y P Int Int
   las cuales representan la funcion constante igual a cero de aridad 1, la funcion
   sucesor de aridad 1 y la funcion proyeccion segundo Int de aridad el
   primer Int, asi como la funcion composicion C que compone el primer
   Descr con la lista [Descr] y la funcion de recursion R con caso base
   el primer Descr y para el resto de casos hacer recursion sobre el
   segundo Descr-}

data Descr = Z | S | P Int Int | C Descr [Descr] | R Descr Descr
             deriving Show

{-Por ultimo, definiremos los datos de entrada los definiremos como
  vectores, que no es mas que una lista de enteros-}

type Vector = [Int]

{-A continuacion, escribiremos los dos ejemplos que usaremos para probar
  nuestro programa de dos formas distintas: Como cadena de caracteres y
  como datos. Esto nos servira tan solo para probar cada funcion por
  separado, ya que el programa sera interactivo y solo tendremos que
  escribirlo tal cual. Los dos ejemplos corresponden a:
  funcion predecesor, cuya descripcion es: C(R(Z,(P 3 2)),[(P 1 1),(P 1 1)])
  y funcion diferencia reducida: R((P 1 1),C(C(R(Z,(P 3 2)),[(P 1 1),(P 1 1)]),[(P 3 3)])) -}

predecesorL = "C(R(Z,(P 3 2)),[(P 1 1),(P 1 1)])"
predecesor = C (R Z (P 3 2)) [(P 1 1), (P 1 1)]
difreducidaL = "R((P 1 1),C(C(R(Z,(P 3 2)),[(P 1 1),(P 1 1)]),[(P 3 3)]))"
difreducida = R a b
         where a = P 1 1
               b = C c f
               c = C d e
               d = R Z (P 3 2)
               e = [a,a]
               f = [P 3 3]

--Mensaje de bienvenida al programa:

bienvenida = "Bienvenido al Interprete para Funciones Recursivas. Para empezar, introduce la descripcion de tu funcion recursiva segun la siguiente notacion: Z para la funcion    identicamente nula, S para la funcion sucesor, (P n j) para la j-esima proyeccion del dato de aridad n (ojo, se pone entre parentesis), C para la composicion        (si vamos a componer g con las funciones h1..hn poner las hi entre corchetes) y R para la recursion primitiva. Es importante que os fijeis en que la primera letra   de la funcion es siempre mayuscula. IMPORTANTE: EVITAR PONER ESPACIOS. Poner solo los que necesita la funcion proyeccion. En cuanto a los vectores, la notacion sera entre corchetes y separando las coordenadas por comas." 


--Programa principal:
                                  
main :: IO ()
main =  do  putStrLn bienvenida
            putStrLn []
            putStrLn "A continuacion, introduzca la descripcion de la funcion"
            xs <- getLine
            putStrLn []
            putStrLn (aridadstring xs)
            putStrLn []
            mostrar xs
            v <- getLine
            mostrar2 v xs


--Funciones de calculo:

{- A continuacion, definiremos las funciones que usaremos para el
   calculo de aridad y de la descripcion sobre un dato-}

--Funcion auxiliar f:

f:: Maybe a -> a
f (Just x) = x

--Funcion que determina la aridad de una descripcion:

aridad:: Descr -> Maybe Int
aridad Z = Just 1
aridad S = Just 1
aridad (P n _) = Just n
aridad (C g (h:hs)) | aridad g == Just (length (h:hs)) && all (== aridad h) (map aridad hs) = aridad h
                    | otherwise = Nothing
aridad (R g h) | fmap (+2) (aridad g) == aridad h = fmap (+1) (aridad g)
               | otherwise = Nothing

{-Funcion que dado un vector ys y una descripcion de una funcion
  devuelve el resultado de aplicar la funcion sobre el vector ys: -}

ejec :: Vector -> Descr -> Maybe Int 
ejec ys Z | length ys == 1 = Just 0
             | otherwise = Nothing
ejec ys S | length ys == 1 = Just (head ys + 1)
             | otherwise = Nothing
ejec ys (P n j) | n<j  || length ys /= n = Nothing
                   | n==j = Just (last ys)
                   | n>j  = Just ((last.take j) ys)
ejec ys (C g hs) = ejec xs g
                  where xs = mapMaybe (ejec ys) hs
ejec ys (R g h) | y == 0 = ejec xs g
                | otherwise = ejec (zs++[f(ejec zs (R g h))]) h 
                 where xs = take (length ys -1) ys
                       y = last ys
                       zs = xs++[y-1]

--Funciones de interpretacion:

{-En este apartado definiremos las funciones que usaremos para
  interpretar la cadena de caracteres dada por el usuario y recibirla
  como un dato del tipo Descr -}

--Funcion que determina que los parentesis esten cerrados:

parentesis1:: (Int,Int) -> String -> Bool
parentesis1 (y,z) [] = z==y
parentesis1 (y,z) (x:xs) | '('==x = parentesis1 (y+1,z) xs
                         | ')'==x = parentesis1 (y,z+1) xs
                         | otherwise = parentesis1 (y,z) xs

parentesis2:: String -> Bool
parentesis2 xs = parentesis1 (0,0) xs

--Funcion que separa los dos argumentos de las funciones C y R:

argumentos:: Int -> String -> String -> Maybe (String,String)
argumentos n ys xs |xs==[] || n<0 = Nothing
argumentos n ys (x:xs) |n==1 && x==',' = Just (tail ys,init xs)
                       |x=='(' || x=='[' = argumentos (n+1) (ys++[x]) xs
                       |x==')' || x==']' = argumentos (n-1) (ys++[x]) xs
                       |otherwise = argumentos n (ys++[x]) xs

argumentos':: String -> Maybe (String,String)
argumentos' xs = argumentos 0 [] xs

{-Funcion que interpreta como dato la cadena de caracteres de la lista
  de funciones del segundo miembro de la funcion C -}

leelista:: Int -> String -> String -> [Descr]
leelista _ xs [] = [string2data xs]
leelista n xs (y:ys) |y=='(' = leelista (n+1) (xs++[y]) ys
                     |y==')' = leelista (n-1) (xs++[y]) ys
                     |y==',' && n==0 = (string2data xs):leelista n [] ys
                     |otherwise = leelista n (xs++[y]) ys 

leelista':: String -> [Descr] 
leelista' xs = leelista 0 [] xs

--Funcion que interpreta el vector que da el usuario

leevector:: String -> String -> Vector
leevector xs [] = [read xs::Int]
leevector xs (y:ys) | y=='[' = leevector xs (init ys)
                    | y==',' = (read xs::Int):leevector [] ys 
                    | otherwise = leevector (xs++[y]) ys 

leevector':: String -> Vector
leevector' xs = leevector [] xs

{-Funcion que interpreta la cadena de caracteres de la descripcion dada
  por el usuario como el tipo de datos Descr -}

string2data:: String -> Descr
string2data "Z" = Z
string2data "S" = S
string2data (a:b:c:xs) |b=='P' = P (read d::Int) (read f::Int)
                         where g = words . init
                               (d,f) = (head (g xs),last (g xs))
string2data (x:xs) | x=='C' = C (string2data ys) (leelista' zs') 
                   | x=='R' = R (string2data ys) (string2data zs)
                   where (ys,zs) = f (argumentos' xs)
                         zs' = init (tail zs)

--Funciones para el programa Main:

{-Estas funciones se usaran simplemente para mostrar por pantalla el
  mensaje adecuado segun el resultado de la funcion aridad y ejec -}
 
aridadstring:: String -> String
aridadstring ys | y == Nothing = "Error. La aridad de la funcion no es correcta"
                | otherwise = "La funcion tiene aridad " ++ show (f y)
                 where y = aridad (string2data ys)  

calcula:: Vector -> Descr -> String
calcula v xs | (ejec v xs) == Nothing = "Error. La aridad del vector no coincide con la aridad de la funcion"
             | otherwise = "El resultado es: " ++ show (f (ejec v xs))

mostrar xs | y == Nothing = putStrLn "Fin del programa. Introduce main para reiniciar el programa"
           | otherwise = putStrLn ("Introduce un vector de aridad " ++ show (f y)) 
              where y = aridad (string2data xs)

mostrar2 v xs | v=="main" = main
              | length w == f y = putStrLn (calcula w ys) 
              | length w /= f y = putStrLn "Vector de aridad incorrecta. Ejecute el programa de nuevo"
                where ys = string2data xs
                      y = aridad ys
                      w = leevector' v

