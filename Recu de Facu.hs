--Recu de Facu
--------- Punto 1.
-- (a->Bool)->(a->a) -> [a]-> [a] ni la mas palida que nombre ponerle
-- f a b c = filter (not . a) c ++ (map b.filter a) c
import Text.Show.Functions
--------- Punto 2.
type Calorias = Int
type Nutriente = String
data Persona = UnaPersona{calorias :: Calorias,nutrientes :: [Nutriente]} deriving (Show)

sofia= UnaPersona 0 []

--------- Punto 3.
type Comida = Persona -> Persona

sumarNutrientes :: Persona -> [Nutriente] ->[Nutriente]
sumarNutrientes unaPersona nutrientesAAgregar = nutrientes unaPersona  ++ nutrientesAAgregar
sumarCalorias :: Persona -> Int -> Int
sumarCalorias unaPersona cantidad = calorias unaPersona + cantidad
tomate :: Comida
tomate unaPersona = unaPersona{
   nutrientes = sumarNutrientes unaPersona ["Vitamina A", "Vitamina C"]
}

zanahoria:: Comida
zanahoria unaPersona = unaPersona{
   nutrientes = sumarNutrientes unaPersona ["Vitamina A","Vitamina C","Vitamina E", "Vitamina K"]
}
carne:: Comida
carne unaPersona = unaPersona{
   calorias = sumarCalorias unaPersona 241,
   nutrientes = sumarNutrientes unaPersona ["Hierro", "Calcio"]
}
data Pan = UnPan{tipo::String,caloriasASumar :: Calorias, nutrientesAAgregar:: [Nutriente]}
blanco = UnPan "Blanco" 265 []
integral = UnPan "Integral" 293 ["Fibras"]
pan:: Pan -> Comida
pan tipoDePan unaPersona = unaPersona{
   calorias = sumarCalorias unaPersona $ caloriasASumar tipoDePan,
   nutrientes = sumarNutrientes unaPersona $ nutrientesAAgregar tipoDePan
}
facturas:: Comida
facturas unaPersona | pipona unaPersona = unaPersona{calorias = sumarCalorias unaPersona 100}
                    | otherwise = unaPersona{ calorias =sumarCalorias unaPersona 5000}

pipona :: Persona -> Bool
pipona unaPersona = calorias unaPersona > 2000 
--------- Punto 4.
ensalada :: Comida
ensalada = tomate.zanahoria --se me repiten algunos nutrientes, como lo cambio?

hamburguesa :: Comida
hamburguesa = carne.tomate.pan blanco
--------- Punto 5.
--comidaCompleta :: [Comida] -> Persona -> Persona
comidaCompleta comidas unaPersona = foldl1 comidas unaPersona --no me estaria funcionando
--------- Punto 6.
sofia2= UnaPersona 2 []
sofia1= UnaPersona 1 []
personasPrueba= [sofia,sofia1,sofia2]
menuPrueba = [facturas,hamburguesa]
--todosSatisfechos:: [Comida] -> [Persona]-> Bool   // que onda ese tipado
todosSatisfechos comidas personas = all satisfecha (map (comidaCompleta comidas) personas)

satisfecha :: Persona ->Bool
satisfecha unaPersona = pipona unaPersona || alMenos5Nutrientes unaPersona

alMenos5Nutrientes :: Persona ->Bool
alMenos5Nutrientes unaPersona = ((>5).length.nutrientes) unaPersona
--------- Punto 7.

--------- Punto 8.
--a. Tendrá las mismas que las que se le definieron a sofia al comienzo ya que haskell no produce efecto colateral
--b. No ya que se continuaria evaluando si todos estan satisfechos, a menos de que al comienzo de la evaluacion se encuentre uno que no cumpla