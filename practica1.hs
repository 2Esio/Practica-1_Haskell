-- Practica 1

import System.Random
import Data.Char

--Ejercicios:

{-
1. Dado el radio de un círculo en unidades u elabora una función
que calcule su circunferencia y la devuelva.

Función para calcular la circunferencia de un círculo dado su radio.
La fórmula para calcular la circunferencia es: circunferencia = 2 * pi * radio

Parámetros:
*radio: El radio del círculo en unidades.

Retorno:
Devuelve la circunferencia del círculo.
-}

circunferencia :: Float -> Float
circunferencia radio = 2 * pi * radio

{-
2. Dado el diámetro de un círculo elabora una función que calcule su área y la devuelva.

Función para calcular el área de un círculo dado su diámetro.
La fórmula para calcular el área es: area = pi * (radio ^ 2)

Parámetros:
*diametro: El diámetro del círculo en unidades.

Retorno:
Devuelve el área del círculo calculada.
-}

areaCirculo :: Float -> Float
areaCirculo diametro = pi * (radio ^ 2)
  where
    radio = diametro / 2

{-
3. Dado b la base y a la altura de un triángulo, calcular su área.


Función para calcular el área de un triángulo dado su base y altura.
La fórmula para calcular el área es: A = (base * altura) / 2

Parámetros:
* base: La base del triángulo en unidades.
* altura: La altura del triángulo en unidades.

Retorno:
Devuelve el área del triángulo calculada.
-}

areaTriangulo :: Float -> Float -> Float
areaTriangulo base altura = (base * altura) / 2

{-
4. Con base en la función anterior y considerando la ecuación común para polígonos regulares de más de 4 lados, elabora una función que sea capaz de obtener el área de la figura respectiva. La función deberá tomar en cuenta los casos de uso donde el área sea negativa, la figura sea menor a 4 lados y si está tiene alguna limitante deberán anotarlo en la documentación de la función. La función deberá recibir un lado y su apotema.
Por ejemplo:
Input: areaPoligono Cuadrado 2 3
Output: 4
Input: areaPoligono Pentagono 6 4
Output: 60
-}

-- | Tipo de dato para representar diferentes polígonos regulares.
data Poligono = Cuadrado | Pentagono | Hexagono | Heptagono | Octagono
              deriving Show

-- | Función para calcular el área de un polígono regular dados su tipo, lado y apotema.
--
--   Parámetros:
--   * poligono: Tipo de polígono (Cuadrado, Pentagono, Hexagono, Heptagono, Octagono).
--   * lado: Longitud del lado del polígono en unidades.
--   * apotema: Longitud de la apotema del polígono en unidades.
--
--   Retorno:
--   Devuelve el área del polígono calculada. En caso de que el área sea negativa,
--   el polígono tenga menos de 4 lados, o haya alguna limitante, se devuelve -1.
areaPoligono :: Poligono -> Float -> Float -> Float
areaPoligono Cuadrado lado _ | lado <= 0 = -1
                             | otherwise = lado ^ 2

areaPoligono Pentagono lado apotema | lado <= 0 || apotema <= 0 = -1
                                   | otherwise = (5 * lado * apotema) / 2

areaPoligono Hexagono lado apotema | lado <= 0 || apotema <= 0 = -1
                                  | otherwise = (3 * sqrt 3 * lado * apotema) / 2

areaPoligono Heptagono _ _ = -1  -- No se puede calcular el área de un heptágono regular con solo la longitud del lado y la apotema.

areaPoligono Octagono lado apotema | lado <= 0 || apotema <= 0 = -1
                                  | otherwise = (2 * (1 + sqrt 2) * lado * apotema)


{-
5. Considera una ecuación de segundo grado. Elabora una función que sea capaz de solucionarla y en su caso, manejar los casos especiales. Explica a detalle que casos logra manejar y como lo realiza.
Por ejemplo:
Input: resolverEcuacionCuadratica 2 (-5) 2
Output: [2, 1/2]
-}

-- | Función para resolver una ecuación cuadrática de segundo grado: ax^2 + bx + c = 0.
--
--   Parámetros:
--   * a: Coeficiente cuadrático.
--   * b: Coeficiente lineal.
--   * c: Término constante.
--
--   Retorno:
--   Devuelve una lista con las soluciones de la ecuación cuadrática. En caso de:
--   - Dos soluciones reales distintas, se devuelven ambas.
--   - Una solución real doble, se devuelve una lista con esa solución duplicada.
--   - No hay soluciones reales, se devuelve una lista vacía.
resolverEcuacionCuadratica :: Float -> Float -> Float -> [Float]
resolverEcuacionCuadratica a b c
  | a == 0 = if b /= 0 then [-c / b] else []  -- Ecuación lineal
  | discriminante > 0 = [(-b + sqrt discriminante) / (2 * a), (-b - sqrt discriminante) / (2 * a)]  -- Dos soluciones reales distintas
  | discriminante == 0 = [-b / (2 * a)]  -- Una solución real doble
  | otherwise = []  -- No hay soluciones reales
  where
    discriminante = b^2 - 4 * a * c


{-
Ejerció extra:
¿Recuerdas el juego de piedra, papel o tijera?
Elabora un script que juegue contra el usuario y devuelva alguno de los tres estados: 
gana usuario, pierde usuario o empate.
Explica su método de uso desde la compilación y/o ejecución y tu estrategia de implementación.
-}

-- Definición de datos para representar las opciones del juego
data Jugada = Piedra | Papel | Tijera deriving (Show, Eq)

-- Función principal del juego
jugarPiedraPapelTijera :: IO ()
jugarPiedraPapelTijera = do
  putStrLn "¡Bienvenido a Piedra, Papel o Tijera!"
  putStrLn "Ingresa tu elección (piedra, papel, tijera):"
  inputUsuario <- getLine

  let eleccionUsuario = parsearJugada inputUsuario
  case eleccionUsuario of
    Just jugadaUsuario -> do
      jugadaMaquina <- generarJugadaAleatoria
      putStrLn $ "La máquina elige: " ++ show jugadaMaquina

      let resultado = determinarGanador jugadaUsuario jugadaMaquina
      putStrLn $ "Resultado: " ++ resultado
    Nothing -> putStrLn "Elección no válida. Ingresa piedra, papel o tijera."

-- Función para generar una jugada aleatoria para la máquina
generarJugadaAleatoria :: IO Jugada
generarJugadaAleatoria = do
  indice <- randomRIO (0 :: Int, 2 :: Int)
  return $ case indice of
    0 -> Piedra
    1 -> Papel
    2 -> Tijera

-- Función para determinar el ganador
determinarGanador :: Jugada -> Jugada -> String
determinarGanador usuario maquina
  | usuario == maquina = "¡Empate!"
  | (usuario == Piedra && maquina == Tijera) ||
    (usuario == Papel && maquina == Piedra) ||
    (usuario == Tijera && maquina == Papel) = "¡Ganas!"
  | otherwise = "¡Pierdes!"

-- Función para parsear la entrada del usuario a una Jugada
parsearJugada :: String -> Maybe Jugada
parsearJugada "piedra" = Just Piedra
parsearJugada "papel"  = Just Papel
parsearJugada "tijera" = Just Tijera
parsearJugada _        = Nothing


-- Función principal que ejecuta el juego
--jugarPiedraPapelTijera
