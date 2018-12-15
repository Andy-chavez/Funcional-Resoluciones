import Text.Show.Functions 

data Elemento = UnElemento { tipo :: String, ataque :: (Personaje-> Personaje), defensa :: (Personaje-> Personaje) } deriving (Show) 
data Personaje = UnPersonaje { nombre :: String, salud :: Float, elementos :: [Elemento], anioPresente :: Int } deriving (Show) 
pepe = UnPersonaje "pepe" 100 [] 2018
-- ataque sobre elemento del rival
-- defensa sobre elemento propio
-- si no dice que es, no importa

-- ################################# PUNTO 1 ################################################
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio unPersonaje = unPersonaje{anioPresente = anio}

meditar :: Personaje -> Personaje
meditar = modificarSalud (*1.5)

causarDanio :: Float -> Personaje -> Personaje
causarDanio danio = modificarSalud (max 0 .flip (-) danio )

modificarSalud :: (Float -> Float) -> Personaje -> Personaje
modificarSalud transformacion personaje = personaje { salud = (transformacion . salud) personaje }

-- ################################# PUNTO 2 ################################################
esMalvado :: Personaje -> Bool
esMalvado= any (esTipo "Malvado").elementos

esTipo :: String ->Elemento -> Bool
esTipo unTipo = ((==unTipo).tipo)

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (esEnemigoMortal personaje) enemigos

esEnemigoMortal :: Personaje -> Personaje -> Bool
esEnemigoMortal personaje enemigo = (any (tieneAtaquePoderoso personaje) . elementos) enemigo

tieneAtaquePoderoso :: Personaje -> Elemento ->Bool
tieneAtaquePoderoso personaje elemento =  danioQueProduce personaje elemento == salud personaje

-- ################################# PUNTO 3 ################################################
noHacerNada :: Personaje -> Personaje
noHacerNada = id

concentracion numero = UnElemento{tipo="Magia", ataque = noHacerNada, defensa= meditarVariasVeces numero}
-- concentracion numero = UnElemento "Magia" noHacerNada (meditarVariasVeces numero)

meditarVariasVeces :: Int -> Personaje -> Personaje
meditarVariasVeces numero = (!! numero) . iterate meditar 

esbirrosMalvados :: Int ->[Elemento] 
esbirrosMalvados numero = replicate numero unEsbirro

unEsbirro = UnElemento "Maldad" (causarDanio 1) noHacerNada

katanaMagica = UnElemento "Magia" (causarDanio 1000) noHacerNada
jack =  UnPersonaje { nombre = "jack", salud = 300 , elementos = [concentracion 3,katanaMagica], anioPresente=200 }

portalAlFuturo anio= UnElemento "Magia" (mandarAlAnio (2800+anio)) (aku (2800+anio).salud)

aku :: Int -> Float -> Personaje
aku anio saludActual = UnPersonaje "aku" saludActual (concentracion 4:portalAlFuturo anio:esbirrosMalvados (anio*100) ) anio

-- ################################# PUNTO 4 ################################################
estaMuerto :: Personaje -> Bool
estaMuerto unPersonaje = salud unPersonaje == 0

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
  | estaMuerto atacante = (defensor,atacante)
  | otherwise 
-- ################################# PUNTO 5 ################################################
