--------------
-- Punto 01 --
--------------

data Chofer = Chofer {
    nombreChofer :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condiciones :: CondicionViaje 
}

type CondicionViaje = Viaje -> Bool

data Viaje = Viaje {
    fecha :: Int,
    cliente :: Cliente,
    costo :: Float
}

data Cliente = Cliente {
    nombreCliente :: String,
    direccion :: String
}

--------------
-- Punto 02 --
--------------

cualquierViaje :: CondicionViaje
cualquierViaje _ = True

viajesDe200 :: CondicionViaje
viajesDe200 = (> 200) . costo

nombreConMasDeXLetras :: Int -> CondicionViaje
nombreConMasDeXLetras nLetras = (> nLetras) . length . nombreCliente . cliente  

noVivaEn :: String -> CondicionViaje
noVivaEn unaZona = (/= unaZona) . direccion . cliente

--------------
-- Punto 03 --
--------------

lucas :: Cliente
lucas = Cliente "Lucas" "Victoria"

daniel :: Chofer
daniel = Chofer "Daniel" 23500 [Viaje 20042017 lucas 150] (noVivaEn "Olivos")

alejandra :: Chofer
alejandra = Chofer "Alejandra" 180000 [] cualquierViaje

--------------
-- Punto 04 --
--------------

puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje unViaje unChofer = condiciones unChofer $ unViaje

--------------
-- Punto 05 --
--------------

liquidacionChofer :: Chofer -> Float
liquidacionChofer = sum . map costo . viajes

--------------
-- Punto 06 --
--------------

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje unViaje = efectuarViaje unViaje . choferConMenosViajes . filter (puedeTomarViaje unViaje)

efectuarViaje ::  Viaje -> Chofer -> Chofer
efectuarViaje unViaje unChofer = unChofer { viajes = unViaje : viajes unChofer }

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes ((elQueMenosViajesHizo chofer1 chofer2):choferes)

elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
elQueMenosViajesHizo chofer1 chofer2
    | (cantidadViajes chofer1) > (cantidadViajes chofer2) = chofer2
    | otherwise                                           = chofer1

cantidadViajes :: Chofer -> Int
cantidadViajes = length . viajes

--------------
-- Punto 07 --
--------------

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

nitoInfo :: Chofer
nitoInfo = Chofer "Nito Infy" 70000 viajeInfinito $ nombreConMasDeXLetras 3

viajeInfinito :: [Viaje]
viajeInfinito = repetirViaje $ Viaje 11032017 lucas 50

-- b) 
-- No termina nunca de calcular la liquidacion 

-- c)
-- Puede realizarlo ya que no involucra a la lista de viajes

--------------
-- Punto 08 --
--------------

gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3
