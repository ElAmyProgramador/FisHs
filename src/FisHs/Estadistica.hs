module FisHs.Estadistica where
import FisHs.Core
import FisHs.Types

newtype Promedio = Promedio Valor deriving Show
newtype Varianza = Varianza Valor deriving Show
newtype Desviacion = Desviacion Valor deriving Show
newtype Incertidumbre = Incertidumbre Valor deriving Show

promedio :: Muestra -> Maybe Promedio
promedio (Muestra []) = Nothing
promedio (Muestra m) =
    let s = sumatoriaV m
        l = Valor $ fromIntegral (longitud m)
    in Promedio <$> divValores s l

varianza :: Muestra -> Maybe Varianza
varianza (Muestra []) = Nothing
varianza (Muestra m) =
    case promedio (Muestra m) of
        Nothing -> Nothing
        Just (Promedio p) ->
            let a = map (\x -> cuadradoV (restaValores x p)) m
                n = Valor (fromIntegral (longitud m))
            in Varianza <$> divValores (sumatoriaV a) n

desvStd :: Muestra -> Maybe Desviacion
desvStd (Muestra []) = Nothing
desvStd (Muestra m) =
    case varianza (Muestra m) of
        Nothing -> Nothing
        Just (Varianza v) ->
            Just $ Desviacion (sqrtV v)

incertidumbreA :: Muestra -> Maybe Incertidumbre
incertidumbreA (Muestra []) = Nothing
incertidumbreA (Muestra m) =
    case desvStd (Muestra m) of
        Nothing -> Nothing
        Just (Desviacion d) ->
            let n = Valor $ sqrt (fromIntegral (longitud m))
            in Incertidumbre <$> (divValores d n)
