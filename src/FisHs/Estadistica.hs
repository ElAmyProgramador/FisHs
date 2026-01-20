module FisHs.Estadistica where

import FisHs.Core
import FisHs.Types

newtype Promedio = Promedio
    {getPromedio :: Valor} deriving (Show, Eq)
newtype Varianza = Varianza
    {getVarianza :: Valor} deriving (Show, Eq)
newtype Desviacion = Desviacion
    {getDesviacion :: Valor} deriving (Show, Eq)
newtype Incertidumbre = Incertidumbre
    {getIncertidumbre :: Valor} deriving (Show, Eq)
newtype Resolucion = Resolucion
    {getResolucion :: Valor} deriving (Show, Eq)

promedio :: Muestra -> Either ErrorFis Promedio
promedio (Muestra []) = Left MuestraVacia
promedio (Muestra m) = do
    let s = sumatoriaV m
        n = Valor $ fromIntegral (longitud m)
    v <- divV s n
    pure $ Promedio v

varianza :: Muestra -> Either ErrorFis Varianza
varianza (Muestra []) = Left MuestraVacia
varianza (Muestra m) = do
    Promedio p <- promedio (Muestra m)
    let a = map (\x -> cuadradoV (restaV x p)) m
        n = Valor $ fromIntegral (longitud m)
    v <- divV (sumatoriaV a) n
    pure $ Varianza v

desvStd :: Muestra -> Either ErrorFis Desviacion
desvStd (Muestra []) = Left MuestraVacia
desvStd (Muestra m) = do
    Varianza v <- varianza (Muestra m)
    pure $ Desviacion (sqrtV v)

incertidumbreA :: Muestra -> Either ErrorFis Incertidumbre
incertidumbreA (Muestra []) = Left MuestraVacia
incertidumbreA (Muestra m) = do
    Desviacion desv <- desvStd (Muestra m)
    let n = Valor $ sqrt (fromIntegral (longitud m))
    di <- divV desv n
    pure $ Incertidumbre di

incertidumbreC :: Incertidumbre -> Incertidumbre -> Incertidumbre
incertidumbreC (Incertidumbre a) (Incertidumbre b) = 
    let u_a = cuadradoV a
        u_b = cuadradoV b
        sumita = sumaV u_a u_b
    in (Incertidumbre $ sqrtV sumita)
