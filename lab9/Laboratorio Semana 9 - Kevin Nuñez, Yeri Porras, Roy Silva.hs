-- Kevin Nuñez
-- Yeri Porras
-- Roy Silva

import Data.Char
import Data.List (find)

-- Ejercicio 2: sumaCuadradosLista
sumaCuadradosLista :: [Int] -> Int
sumaCuadradosLista xs = sum (map (^2) xs)

-- Ejercicio 2: cuentaPares
cuentaPares :: [Int] -> Int
cuentaPares xs = length (filter even xs)

-- Ejercicio 2: pares menores a 20
paresMenoresA20 :: [Int]
paresMenoresA20 = [x | x <- [1..20], (\n -> n `mod` 2 == 0) x] -- profe no me acuerdo si habia que incluir el 20 :(

-- Ejercicio 2: menuEjercicio2
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
            putStrLn("La suma de los cuadrados de los números ingresados es: ")
            print(sumaCuadradosLista numeros)
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

-- Ejercicio 3: estructura de contacto
data Contacto = Contacto {
    nombre :: String,
    telefono :: String,
    correo :: String,
    edad :: Int
} deriving (Show)


type Contactos = [Contacto]

--Agregar contacto
agregarContacto :: Contacto -> Contactos -> Contactos
agregarContacto nuevo contactoLista = 
    case find (\c -> nombre c == nombre nuevo) contactoLista of
        Just _  -> contactoLista
        Nothing -> nuevo : contactoLista

--Buscar Telefono
buscarTelefono :: String -> Contactos -> String
buscarTelefono nombreContacto contactos = 
    case find (\c -> nombre c == nombreContacto) contactos of
        Just contacto -> "Telefono de " ++ nombreContacto ++ ": " ++ telefono contacto
        Nothing       -> "Contacto no encontrado"

--Ver contactos
verContactos :: Contactos -> IO ()
verContactos [] = putStrLn "No hay contactos"
verContactos contactos = do
    putStrLn "Lista de contactos:"
    mapM_ (\c -> putStrLn (nombre c ++ " - " ++ telefono c ++ " - " ++ correo c ++ " - " ++ show (edad c))) contactos

--Eliminar contacto
eliminarContacto :: String -> Contactos -> Contactos
eliminarContacto nombreContacto contactos = 
    filter (\c -> nombre c /= nombreContacto) contactos

-- menuEjercicio3
menuEjercicio3 :: Contactos -> IO ()
menuEjercicio3 listaContactos = do
    putStrLn "=== Menu de ejercicio 3 ==="
    putStrLn "1. Agregar contacto"
    putStrLn "2. Buscar teléfono"
    putStrLn "3. Ver Todos los contactos"
    putStrLn "4. Eliminar contacto"
    putStrLn "5. Volver al menu principal"
    putStrLn "Seleccione una opcion: "
    opcionEjercicio2 <- getLine
    case opcionEjercicio2 of
        "1" -> do
            putStrLn "Ingrese su nombre:"
            nombre <- getLine
            putStrLn "Ingrese su número de teléfono:"
            telefono <- getLine
            putStrLn "Ingrese su correo electrónico:"
            correo <- getLine
            putStrLn "Ingrese su edad:"
            edadStr <- getLine
            let edad = read edadStr :: Int
            let nuevoContacto = Contacto nombre telefono correo edad
            let nuevaLista = agregarContacto nuevoContacto listaContactos  -- Nombre diferente
            putStrLn ("Contacto agregado: " ++ nombre)
            verContactos nuevaLista
            menuEjercicio3 nuevaLista
        "2" -> do
            putStrLn "Ingrese el nombre del contacto a buscar:"
            nombreContacto <- getLine
            let telefonoEncontrado = buscarTelefono nombreContacto listaContactos
            putStrLn (telefonoEncontrado)
            menuEjercicio3 listaContactos   
        "3" -> do
            verContactos listaContactos
            menuEjercicio3 listaContactos 
        "4" -> do
            putStrLn "Ingrese el nombre del contacto a eliminar:"
            nombreContacto <- getLine
            let nuevaLista = eliminarContacto nombreContacto listaContactos  -- Nombre diferente
            putStrLn ("Contacto eliminado: " ++ nombreContacto)
            verContactos nuevaLista
            menuEjercicio3 nuevaLista
        "5" -> do
            putStrLn "Volviendo al menu principal..."
            menu
        _   -> do
            putStrLn "Opcion invalida"
            menuEjercicio3 listaContactos

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
            let contactosIniciales :: Contactos
                contacto1 = Contacto "Kevin" "86454511" "kevin0506@gmail.com" 25
                contacto2 = Contacto "Yeri" "88451236" "yeri@gmail.com" 24
                contacto3 = Contacto "Roy" "87896541" "roy@gmail.com" 26
                contacto4 = Contacto "Bryan" "80014587" "bryan@gmail.com" 23
                contacto5 = Contacto "Marlen" "85874125" "marlen@gmail.com" 27
                contactosIniciales = [contacto1, contacto2, contacto3, contacto4, contacto5]
            menuEjercicio3 contactosIniciales
        "3" -> do
            putStrLn "Saliendo..."
            return ()
        _   -> do
            putStrLn "Opcion invalida"
            menu

main :: IO ()
main = menu
