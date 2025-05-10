import Data.Char

-- Ejercicio 2: sumaCuadradosLista
sumaCuadradosLista :: [Int] -> Int
sumaCuadradosLista xs = sum (map (^2) xs)

-- Ejercicio 2: cuentaPares
cuentaPares :: [Int] -> Int
cuentaPares xs = length (filter even xs)

-- Ejercicio 2: pares menores a 20
paresMenoresA20 :: [Int]
paresMenoresA20 = [x | x <- [1..20], (\n -> n `mod` 2 == 0) x] -- profe no me acuerdo si habia que incluir el 20 :(

menuEjercicio2 :: IO ()
menuEjercicio2 = do
    putStrLn "=== Menu de ejercicio 2 ==="
    putStrLn "1. Saludar usuario"
    putStrLn "2. Suma de cuadrados de una lista"
    putStrLn "3. Contar pares"
    putStrLn "4. Pares menores a 20"
    putStrLn "5. Volver al menu principal"
    putStrLn "Seleccione una opcion: "
    opcionEjercicio2 <- getLine
    case opcionEjercicio2 of
        "1" -> do
            putStrLn "Ingrese su nombre:"
            nombre <- getLine
            putStrLn ("¡Hola " ++ nombre ++ "!")
            menuEjercicio2
        "2" -> do
            putStrLn "Ingrese los numeros separados por espacios:"
            entrada <- getLine
            let numeros = map read (words entrada) :: [Int]
            print (sumaCuadradosLista numeros)
            menuEjercicio2
        "3" -> do
            putStrLn "Ingrese los numeros separados por espacios:"
            entrada <- getLine
            let numeros = map read (words entrada) :: [Int]
            print (cuentaPares numeros)
            menuEjercicio2
        "4" -> do
            print paresMenoresA20
            menuEjercicio2
        "5" -> do
            putStrLn "Volviendo al menu principal..."
            menu
        _   -> do
            putStrLn "Opcion invalida"
            menuEjercicio2

menu :: IO ()
menu = do
    putStrLn "=== Menu principal ==="
    putStrLn "1. Ejercicio 2"
    putStrLn "2. Ejercicio 3"
    putStrLn "3. Salir"
    putStrLn "Seleccione una opcion:"
    opcion <- getLine
    case opcion of
        "1" -> do
            menuEjercicio2
        "2" -> do
            menu
        "3" -> putStrLn "Cerrando el programa..."
        _   -> do
            putStrLn "Opcion invalida"
            menu

main :: IO ()
main = menu
