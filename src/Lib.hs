module Lib () where

import Data.List (genericLength)
import Text.Show.Functions ()

data Propiedad = UnaPropiedad
  { nombrePropiedad :: String,
    precio :: Float
  }
  deriving (Show)

data Participante = UnParticipante
  { nombre :: String,
    dinero :: Float,
    tacticaDeJuego :: String,
    propiedadesCompradas :: [Propiedad],
    acciones :: [Accion]
  }
  deriving (Show)

type Accion = Participante -> Participante

carolina :: Participante
carolina = UnParticipante "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Participante
manuel = UnParticipante "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse]

-- pasarPorElBanco: aumenta el dinero del jugador en $40 y cambia su táctica a “Comprador compulsivo”.
pasarPorElBanco :: Accion
pasarPorElBanco participante = (cambiarTactica . aumentarDinero 40) participante

aumentarDinero :: Float -> Participante -> Participante
aumentarDinero cantidad participante = participante {dinero = dinero participante + cantidad}

cambiarTactica :: Participante -> Participante
cambiarTactica participante = participante {tacticaDeJuego = "Comprador compulsivo"}

-- enojarse: suma $50 y agrega gritar a sus acciones.

enojarse :: Accion
enojarse participante = (agregarAccion gritar . aumentarDinero 50) participante

agregarAccion :: Accion -> Participante -> Participante
agregarAccion accion participante = participante {acciones = acciones participante ++ [accion]}

-- gritar: agrega “AHHHH” al principio de su nombre.

gritar :: Accion
gritar participante = participante {nombre = "AHHHH" ++ nombre participante}

-- subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o “Accionista” podrán ganar la propiedad. Ganar implica restar el precio de la propiedad de su dinero y sumar la nueva adquisición a sus propiedades.
subastar :: Propiedad -> Accion
subastar propiedad participante
  | tacticaDeJuego participante == "Oferente singular" = ganarPropiedad propiedad participante
  | tacticaDeJuego participante == "Accionista" = ganarPropiedad propiedad participante
  | otherwise = participante

ganarPropiedad :: Propiedad -> Participante -> Participante
ganarPropiedad propiedad participante = (agregarPropiedad propiedad . restarPrecioPropiedad propiedad) participante

restarPrecioPropiedad :: Propiedad -> Participante -> Participante
restarPrecioPropiedad propiedad participante = participante {dinero = dinero participante - (precio propiedad)}

agregarPropiedad :: Propiedad -> Participante -> Participante
agregarPropiedad propiedad participante = participante {propiedadesCompradas = propiedadesCompradas participante ++ [propiedad]}

-- cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida. Las propiedades baratas son aquellas cuyo precio es menor a $150.

cobrarAlquileres :: Accion
cobrarAlquileres participante = sumarPropiedadesBaratas . sumerPropiedadesCaras $ participante

sumarPropiedadesBaratas :: Participante -> Participante
sumarPropiedadesBaratas participante = participante {dinero = dinero participante + 10 * (genericLength (propiedadesBaratas participante))}

propiedadesBaratas :: Participante -> [Propiedad]
propiedadesBaratas participante = filter esPropiedadBarata (propiedadesCompradas participante)

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata propiedad = precio propiedad < 150

sumerPropiedadesCaras :: Participante -> Participante
sumerPropiedadesCaras participante = participante {dinero = dinero participante + 20 * (genericLength (propiedadesCaras participante))}

propiedadesCaras :: Participante -> [Propiedad]
propiedadesCaras participante = filter (not . esPropiedadBarata) (propiedadesCompradas participante)

-- pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, en ese caso suma $200.

pagarAAccionistas :: Accion
pagarAAccionistas participante
  | esAccionista participante = aumentarDinero 200 participante
  | otherwise = sacarDinero 100 participante

esAccionista :: Participante -> Bool
esAccionista (UnParticipante _ _ tacticaDeJuego _ _) = tacticaDeJuego == "Accionista"

sacarDinero :: Float -> Participante -> Participante
sacarDinero cantidad participante = participante {dinero = dinero participante - cantidad}