module FisHs.Estadistica where

import FisHs.Core
import FisHs.Types

newtype Promedio = Promedio Valor deriving Show
newtype Varianza = Varianza Valor deriving Show
newtype Desviacion = Desviacion Valor deriving Show
newtype Incertidumbre = Incertidumbre Valor deriving Show
newtype Resolucion = Resolucion Valor deriving Show

promedio :: Muestra -> Either ErrorFis Promedio
promedio (Muestra []) = Left MuestraVacia
promedio (Muestra m) = do
    let s = sumatoriaV m
        n = Valor $ fromIntegral (longitud m)
    v <- divV s n
    return $ Promedio v

varianza :: Muestra -> Either ErrorFis Varianza
varianza (Muestra []) = Left MuestraVacia
varianza (Muestra m) = do
    Promedio p <- promedio (Muestra m)
    let a = map (\x -> cuadradoV (restaV x p)) m
        n = Valor $ fromIntegral (longitud m)
    v <- divV (sumatoriaV a) n
    return $ Varianza v

desvStd :: Muestra -> Either ErrorFis Desviacion
desvStd (Muestra []) = Left MuestraVacia
desvStd (Muestra m) = do
    Varianza v <- varianza (Muestra m)
    return $ Desviacion (sqrtV v)

incertidumbreA :: Muestra -> Either ErrorFis Incertidumbre
incertidumbreA (Muestra []) = Left MuestraVacia
incertidumbreA (Muestra m) = do
    Desviacion desv <- desvStd (Muestra m)
    let n = Valor $ sqrt (fromIntegral (longitud m))
    di <- divV desv n
    return $ Incertidumbre di

incertidumbreC :: Incertidumbre -> Incertidumbre -> Incertidumbre
incertidumbreC (Incertidumbre a) (Incertidumbre b) = 
    let u_a = cuadradoV a
        u_b = cuadradoV b
        sumita = sumaV u_a u_b
    in (Incertidumbre $ sqrtV sumita)
