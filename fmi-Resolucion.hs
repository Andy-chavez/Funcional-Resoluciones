-- FMI 
import Text.Show.Functions
------------------------------------- 1.
type Recurso = String
data Pais = UnPais {ingreso :: Float,activosPublico :: Int, activosPrivado :: Int, recursosNaturales :: [Recurso], deuda :: Float} deriving (Eq, Show)
namibia :: Pais
namibiaPunto1 = UnPais 4140 400000 650000 ["Mineria","Ecoculturismo"] 50
namibia = UnPais 4140 400000 650000 recursosNaturalesInfinitos 50
------------------------------------- 2.
type Estrategia = Pais -> Pais

prestarMillonesA :: Float -> Estrategia
prestarMillonesA cantidad unPais = unPais{ deuda = deuda unPais + cobrarIntereses cantidad*1000000 } 

cobrarIntereses:: Float -> Float
cobrarIntereses cuanto = cuanto * 1.5

reducirCantidadPuestosPublicos :: Int -> Estrategia
reducirCantidadPuestosPublicos cantidad unPais = unPais{ 
   activosPublico = (activosPublico unPais) - cantidad,
   ingreso = ingreso unPais *(1 - disminuirIngreso (activosPublico unPais))
}

disminuirIngreso :: Int -> Float
disminuirIngreso cantidadPuestos
  | cantidadPuestos > 100 = 0.2
  | otherwise = 0.15

explotar :: Recurso -> Estrategia
explotar recurso unPais = unPais{
   deuda = deuda unPais - 2000000, 
   recursosNaturales = quitarRecurso recurso $ recursosNaturales unPais }

quitarRecurso :: Recurso -> [Recurso] -> [Recurso]
quitarRecurso recurso recursosDelPais = filter (/= recurso) recursosDelPais

blindaje :: Estrategia
blindaje unPais = (prestarMillonesA (pbi unPais *0.5). reducirCantidadPuestosPublicos 500) unPais

pbi :: Pais -> Float
pbi unPais = ingreso unPais * fromIntegral (poblacionActiva unPais)

poblacionActiva :: Pais -> Int
poblacionActiva unPais = activosPublico unPais + activosPrivado unPais

------------------------------------- 3. 
prestarYExplotar :: [Estrategia]
prestarYExplotar  = [explotar "Mineria", prestarMillonesA 200 ]

aplicarReceta receta unPais = foldr1 receta unPais -- sino puedo hacer foldr ($) pais receta
------------------------------------- 4. // ORDEN SUPERIOR,APLICACION PARCIAL Y COMPOSICION
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter $ elem "Petroleo" . recursosNaturales

deudaTotal :: [Pais] -> Float
deudaTotal =  foldr ((+) . deuda) 0
------------------------------------- 5. // RECURSIVIDAD
--estaOrdenado :: Pais -> [Estrategia] -> Bool
--estaOrdenado pais [receta] = True
--estaOrdenado pais (receta1:receta2:recetas) = (pbi . aplicarReceta receta1 pais) <= (pbi . aplicarReceta receta2 pais )&& estaOrdenado pais (receta2:recetas)  
------------------------------------- 6. // TEORICO
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos
pruebaInfinita1 = puedenZafar [namibia, namibiaPunto1]
--              no termina nunca, porque quiere buscar "Mineria" entre los recursos
pruebaInfinita2 = deudaTotal [namibia, namibiaPunto1]
--              se puede porque al no evaluar los recursos solamente suma deuda
-- relacionado con la lazy evaluation, solo se evalua lo que se necesita
