module FisHs.Estadistica where
import FisHs.Core
import FisHs.Types

newtype Promedio = Promedio Valor deriving Show
newtype Varianza = Varianza Valor deriving Show
newtype Desviacion = Desviacion Valor deriving Show
newtype Incertidumbre = Incertidumbre Valor deriving Show
newtype Resolucion = Resolucion Valor deriving Show

promedio :: Muestra -> Maybe Promedio
promedio (Muestra []) = Nothing
promedio (Muestra m) =
    let s = sumatoriaV m
        l = Valor $ fromIntegral (longitud m)
    in Promedio <$> divV s l

varianza :: Muestra -> Maybe Varianza
varianza (Muestra []) = Nothing
varianza (Muestra m) = do
    Promedio p <- promedio (Muestra m)
    let a = map (\x -> cuadradoV (restaV x p)) m
        n = Valor $ fromIntegral (longitud m)
    v <- divV (sumatoriaV a) n
    return $ Varianza v

desvStd :: Muestra -> Maybe Desviacion
desvStd (Muestra []) = Nothing
desvStd (Muestra m) = do
    Varianza v <- varianza (Muestra m)
    return $ Desviacion (sqrtV v)

incertidumbreA :: Muestra -> Maybe Incertidumbre
incertidumbreA (Muestra []) = Nothing
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
