import Text.Show.Functions
-------------------------------------------- 1. --------------------------------------------------
data Animal = UnAnimal{ coeficiente:: Int, especie:: String, capacidades::[String]} deriving (Show)

pinky = UnAnimal 10 "Raton" []
cerebro = UnAnimal 180 "Raton" []
type Transformacion = Animal -> Animal
-------------------------------------------- 2. --------------------------------------------------
inteligenciaSuperior :: Int -> Transformacion
inteligenciaSuperior cantidadAAumentar unAnimal = unAnimal{coeficiente = coeficiente unAnimal + cantidadAAumentar}

pinkificar :: Transformacion
pinkificar unAnimal = unAnimal{ capacidades = []}

superpoderes :: Transformacion
superpoderes unAnimal 
   |verificarEspecie "elefante" unAnimal = unAnimal{ capacidades = capacidades unAnimal ++ ["No tenerle miedo a los ratones"]}
   |verficarRaton unAnimal = unAnimal{capacidades= capacidades unAnimal ++ ["hablar"]}
   |otherwise = unAnimal

verificarEspecie :: String -> Criterio
verificarEspecie especieComparar = (==especieComparar).especie

verficarRaton :: Animal -> Bool
verficarRaton unAnimal = verificarEspecie "raton" unAnimal && verificarCoeficienteMayorA 100 unAnimal

verificarCoeficienteMayorA :: Int -> Criterio
verificarCoeficienteMayorA coeficienteComparar = (>coeficienteComparar).coeficiente

-------------------------------------------- 3. --------------------------------------------------
type Criterio = Animal -> Bool

antropomorfico :: Criterio
antropomorfico unAnimal = poseeHabilidad "hablar" unAnimal && verificarCoeficienteMayorA 60 unAnimal

poseeHabilidad :: String -> Criterio
poseeHabilidad habilidadBuscada = ((elem habilidadBuscada).capacidades)

noTanCuerdo :: Criterio
noTanCuerdo = ((>2).length.filter pinkiesco.capacidades)

-------------------------------------------- BONUS A. --------------------------------------------------
pinkiesco :: String -> Bool
pinkiesco habilidad = largoMenorA 11 habilidad && comienzaCon "hacer" habilidad

largoMenorA :: Int -> String -> Bool
largoMenorA cantidadLetras = (<cantidadLetras).length

comienzaCon :: String -> String -> Bool
comienzaCon palabraBuscada = (== palabraBuscada).take 5
-------------------------------------------- 4. --------------------------------------------------
data Experimento = UnExperimento{ transformaciones:: [Transformacion], criterioDeExito :: Criterio} deriving (Show)

experimentoExitoso :: Experimento-> Animal -> Bool
experimentoExitoso unExperimento = (criterioDeExito unExperimento).(aplicarExperimento unExperimento) 

aplicarExperimento :: Experimento -> Transformacion
aplicarExperimento unExperimento = foldl1 (.) (transformaciones unExperimento)

ratonEj4 = UnAnimal 10 "Raton" ["Destruenglonir","Hacer planes desalmados"]
experimentoEj4 = UnExperimento [pinkificar, inteligenciaSuperior 10, superpoderes] antropomorfico
-------------------------------------------- 5. --------------------------------------------------
{-reporte1 :: [Animal] -> [String] -> Experimento -> [Int]
reporte1 animales habilidades unExperimento = map coeficiente (filtrarPor  )
reporte2 :: [Animal] -> [String] -> Experimento -> [Int]
animales habilidades unExperimento =
reporte3 :: [Animal] -> [String] -> Experimento -> [Int]
animales habilidades unExperimento =
-}
tieneElementoDeLista :: [Capacidades] -> Capacidades->Bool
tieneElementoDeLista listaDeCapacidades unaCapacidad = elem unaCapacidad listaDeCapacidades -- Delegacion innecesaria? tal vez

estaAlgunaCapacidad :: [Capacidades] -> Animal ->Bool
estaAlgunaCapacidad listaDeCapacidades unAnimal = any (tieneElementoDeLista (capacidades unAnimal)) listaDeCapacidades

reporte1 :: [Animal]-> [Capacidades]->Experimento-> [Int]
reporte1 listaDeAnimales listaDeCapacidades unExperimento = map coefIntelectual (filter ((estaAlgunaCapacidad listaDeCapacidades).(aplicarExperimento unExperimento)) listaDeAnimales)
  
reporte3 ::[Animal]-> [Capacidades]->Experimento-> [Int]
reporte3 listaDeAnimales listaDeCapacidades unExperimento=map length (map capacidades (filter (not.(estaAlgunaCapacidad listaDeCapacidades).(aplicarExperimento unExperimento)) listaDeAnimales))

estanTodas:: [Capacidades] -> Animal ->Bool
estanTodas listaDeCapacidades unAnimal = all (tieneElementoDeLista (capacidades unAnimal)) listaDeCapacidades

reporte2  :: [Animal]-> [Capacidades]->Experimento-> [String]
reporte2 listaDeAnimales listaDeCapacidades unExperimento  = map especie (filter ((estanTodas listaDeCapacidades).(aplicarExperimento unExperimento)) listaDeAnimales)

-------------------------------------------- 6. --------------------------------------------------

