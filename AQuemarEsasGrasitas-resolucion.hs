-- AQuemarEsasGrasitas
import Text.Show.Functions 

type Edad = Int
type Peso= Int
type Tonificacion= Int
type Persona = (Edad, Peso, Tonificacion)
pancho = (40, 120,1)
andres= (22,80,6)

edad (e,_,_) = e
peso (_,p,_) = p 
tonificacion (_,_,t)=t

type Ejercicio = Persona -> Int -> Persona

relax :: Ejercicio
relax persona minutos = persona
------------------------------------------ 1. ---------------------------------------------------------
saludable :: Persona -> Bool
saludable unaPersona = noObesa unaPersona && tonificacionMayorA 5 unaPersona

tonificacionMayorA :: Int -> Persona -> Bool
tonificacionMayorA minimo= (>minimo).tonificacion 

noObesa :: Persona -> Bool
noObesa = (<=100).peso
------------------------------------------ 2. ---------------------------------------------------------
quemarCalorias :: Ejercicio
quemarCalorias (e,p,t) calorias
  |not $ noObesa (e,p,t) = (e,p-(div calorias 150),t)
  |noObesa (e,p,t) && e>30 && calorias>200 = (e,p-1,t)
  |otherwise = (e,p-( div calorias (p*e)),t)

------------------------------------------ 3. ---------------------------------------------------------
caminata :: Ejercicio
caminata unaPersona minutos = quemarCalorias unaPersona (5*minutos)

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta unaPersona minutos = quemarCalorias unaPersona ((6+ (div minutos 5) )*minutos)

pesas :: Int -> Ejercicio
pesas kilosALevantar (e,p,t) minutos
  |minutos>10 = (e,p,t+ div kilosALevantar 10)
  |otherwise = (e,p,t)

colina :: Int -> Ejercicio
colina inclinacion unaPersona minutos = quemarCalorias unaPersona (2*minutos*inclinacion)

montaña :: Int -> Ejercicio
montaña inclinacion unaPersona minutos = (colina (inclinacion+3) (colina inclinacion unaPersona (div minutos 2)) (div minutos 2))
------------------------------------------ 4. ---------------------------------------------------------
type Rutina= (Nombre,Duracion,[Ejercicio]) = .-.


------------------------------------------ 5. ---------------------------------------------------------
