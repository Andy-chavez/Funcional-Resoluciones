-- Huber 2.0
import Text.Show.Functions
------------------------------------------ 1. ---------------------------------------------------------

data Chofer = UnChofer{ nombreChofer::String, kilometraje :: Int, viajes :: [Viaje], condicion :: CondicionViaje} deriving (Show)
data Viaje = UnViaje{fecha :: (Int,Int,Int), cliente:: Cliente, costo :: Int} deriving (Show)
data Cliente = UnCliente{ nombreCliente ::String, vivienda :: String} deriving (Show)
type CondicionViaje = Viaje -> Bool
------------------------------------------ 2. ---------------------------------------------------------

tomaCualquiera :: CondicionViaje
tomaCualquiera unViaje = True

viajeDeMasDe200 :: CondicionViaje
viajeDeMasDe200 unViaje = viajeMasCaroQue 200 unViaje

viajeMasCaroQue :: Int -> Viaje -> Bool
viajeMasCaroQue comparativo = (>comparativo).costo

clienteConNLetras :: Int -> CondicionViaje
clienteConNLetras cantidadLetras = (>cantidadLetras).length.nombreClienteDeViaje

nombreClienteDeViaje :: Viaje -> String
nombreClienteDeViaje = nombreCliente.cliente

noVaAZona :: String -> CondicionViaje
noVaAZona zona = (/= zona).vivienda.cliente
------------------------------------------ 3. ---------------------------------------------------------
lucas = UnCliente "Lucas" "Victoria"
daniel = UnChofer "Daniel" 23500 [viajeConLucas] (noVaAZona "Olivos")
viajeConLucas = UnViaje (20,04,17) lucas 150
alejandra = UnChofer "Alejandra" 180000 [] tomaCualquiera
------------------------------------------ 4. ---------------------------------------------------------
choferPuedeTomarViaje :: Viaje -> Chofer -> Bool
choferPuedeTomarViaje unViaje unChofer = condicion unChofer unViaje
------------------------------------------ 5. ---------------------------------------------------------
{-liquidacion :: Chofer -> Int
liquidacion unChofer = sum (liquidacionDeViajes unChofer)

liquidacionDeViajes :: Chofer -> [Int]
liquidacionDeViajes unChofer = map costo (viajes unChofer)
-}
liquidacionChofer :: Chofer -> Int
liquidacionChofer chofer = foldr ((+) . costo) 0 (viajes chofer) 
-- En estos casos a veces conviene usar el fold ya que facilita un poco todo, y conviene poner el seed ya que sino el error de tipos es groso

------------------------------------------ 6. ---------------------------------------------------------
realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje unViaje = (hacerViaje unViaje).choferConMenosViajes.filter (choferPuedeTomarViaje unViaje)

cantidadViajes :: Chofer -> Int
cantidadViajes = length.viajes

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes ((elQueMenosViajesHizo chofer1 chofer2):choferes)

elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
elQueMenosViajesHizo chofer1 chofer2 
   | cantidadViajes chofer1 > cantidadViajes chofer2 = chofer2 
   | otherwise = chofer1
{-interesante esta forma de resolver lo del chofer con menos viajes -}
hacerViaje :: Viaje -> Chofer -> Chofer
hacerViaje unViaje unChofer = unChofer{viajes= viajes unChofer ++ [unViaje] }
------------------------------------------ 7. ---------------------------------------------------------
nitoInfy = UnChofer "Nito Infy" 70000 (viaje : repetirViaje viaje) (clienteConNLetras 3)
viaje = UnViaje (11,03,17) lucas 50
repetirViaje viaje = viaje : repetirViaje viaje
-- b) No se puede calcular ya que es infinita
-- c) Si se puede realizar ya que tendra en cuenta su condicion y no su cantidad de viajes
------------------------------------------ 8. ---------------------------------------------------------
gongNeng :: Ord a=> a-> (a->Bool) -> (b->a) -> [b] ->a
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3

