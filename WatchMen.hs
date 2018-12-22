import Text.Show.Functions
type Vigilante = (String,[String],Int)
type Evento = [Vigilante] -> [Vigilante]

algunosVigilantes = [ ("El Comediante", ["Fuerza"], 1942), ("Buho Nocturno", ["Lucha", "Ingenierismo"], 1963), ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964), ("Espectro de Seda", ["Lucha", "Sigilo", "Fuerza"], 1962), ("Ozimandias", ["Inteligencia", "Mas Inteligencia Aun"], 1968), ("Buho Nocturno", ["Lucha", "Inteligencia", "Fuerza"], 1939), ("Espectro de Seda", ["Lucha", "Sigilo"], 1940),("Rorschach",["Lucha"],1950),("Dr Manhattan",["Sigilo"],1954)]
agentesDelGobierno = [("Jack Bauer","24"), ("El Comediante", "Watchmen"), ("Dr. Manhattan", "Watchmen"), ("Liam Neeson", "Taken")]

anio (_,_,a) = a
nombre (n,_,_) = n
habilidades (_,h,_) = h

destruccionDeNiushork :: Evento
destruccionDeNiushork = (retiroOMuerte "Dr. Manhattan").(retiroOMuerte "Rorschach")

retiroOMuerte :: String -> Evento
retiroOMuerte nombreDelQueMuere vigilantes = filter ((/=nombreDelQueMuere).nombre) vigilantes

guerraDeVietnam :: Evento
guerraDeVietnam = map (agregarHabilidadesALosAgentes "Cinismo")

agregarHabilidades :: String -> Vigilante -> Vigilante
agregarHabilidades habilidad (nombre,habilidades,anio) = (nombre,habilidades ++ [habilidad],anio)

agregarHabilidadesALosAgentes :: String -> Vigilante -> Vigilante
agregarHabilidadesALosAgentes habilidad vigilante
  |esAgente $ nombre vigilante = agregarHabilidades habilidad vigilante
  | otherwise = vigilante

esAgente :: String -> Bool
esAgente nombre = any ((== nombre).fst) agentesDelGobierno

accidenteDeLab :: Int -> Evento
accidenteDeLab anio vigilantes = vigilantes ++ [("Dr Manhattan", ["Manipulacion de Materia"], anio)]

actaDeKeene :: Evento
actaDeKeene vigilantes = filtrarPor tieneSucesor vigilantes

tieneSucesor :: [Vigilante] -> Vigilante -> Bool
tieneSucesor vigilantes vigilante = any (sucesor vigilante) vigilantes

sucesor :: Vigilante -> Vigilante -> Bool
sucesor unVigilante otroVigilante = nombre unVigilante == nombre otroVigilante && anio unVigilante > anio otroVigilante

haciendoHistoria :: [Evento]
haciendoHistoria = [destruccionDeNiushork,retiroOMuerte "El comediante",guerraDeVietnam , accidenteDeLab 1959,actaDeKeene ]

desarrolloDeHistoria :: [Evento] -> Evento
desarrolloDeHistoria eventos vigilantes = concatenacionEventos eventos vigilantes

concatenacionEventos:: [Evento] -> Evento
concatenacionEventos eventos = foldl1 (.) eventos

nombreDelSalvador :: [Vigilante] -> String
nombreDelSalvador = (nombre.head.ordenarPorMasHabilidosos.destruccionDeNiushork)

filtrarPor :: ([Vigilante] -> Vigilante -> Bool)->[Vigilante] -> [Vigilante]
filtrarPor funcion vigilantes= filter (funcion vigilantes) vigilantes

ordenarPorMasHabilidosos :: Evento
ordenarPorMasHabilidosos vigilantes= filtrarPor masHabilidoso vigilantes

masHabilidoso :: [Vigilante] -> Vigilante -> Bool
masHabilidoso listaDeVigilantes unVigilante = all (<=(length (habilidades unVigilante))) (obtenerLargoHabs listaDeVigilantes)

obtenerLargoHabs :: [Vigilante] -> [Int]
obtenerLargoHabs = map (length.habilidades)

elElegido :: [Vigilante] -> String
elElegido = head.habilidades.head.vigilanteConNombreMasLargo.guerraDeVietnam

vigilanteConNombreMasLargo :: [Vigilante] -> [Vigilante]
vigilanteConNombreMasLargo vigilantes = filtrarPor nombreMasLargo vigilantes

nombreMasLargo :: [Vigilante] -> Vigilante -> Bool
nombreMasLargo listaDeVigilantes unVigilante = all (<=(length (nombre unVigilante))) (obtenerLargoNombre listaDeVigilantes)

obtenerLargoNombre :: [Vigilante] -> [Int]
obtenerLargoNombre = map (length.nombre)

obtenerAnio :: [Vigilante] -> [Int]
obtenerAnio = map anio

patriarca :: [Vigilante] -> Int
patriarca vigilantes = (-) 2018 $ (anio.head.vigilanteMasAntiguo.actaDeKeene) vigilantes

vigilanteMasAntiguo :: [Vigilante] -> [Vigilante]
vigilanteMasAntiguo vigilantes = filtrarPor masViejo vigilantes

masViejo :: [Vigilante] -> Vigilante -> Bool
masViejo listaDeVigilantes unVigilante = all (<=(anio unVigilante)) (obtenerAnio listaDeVigilantes)