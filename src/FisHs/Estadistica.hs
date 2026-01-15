module FisHs.Estadistica where
import FisHs.Core
import FisHs.Types

type Muestra = [Double]

newtype Promedio = Promedio Double deriving Show
newtype Varianza = Varianza Double deriving Show
newtype Desviacion = Desviacion Double deriving Show
newtype Incertidumbre = Incertidumbre Double deriving Show

-- lista de Double (o Muestra)

promedio :: Muestra -> Maybe Promedio
promedio [] = Nothing
promedio m =
    let l = longitud m
        s = sumatoria m
    in Just $ Promedio (s / fromInteger l)

varianza :: Muestra -> Maybe Varianza
varianza [] = Nothing
varianza m =
    case promedio m of
        Nothing -> Nothing
        Just (Promedio p) ->
            let a = map (\x -> (x - p)^2) m
                b = (fromInteger $ longitud a) - 1
            in Just $ Varianza (sumatoria a / b)

desvStd :: Muestra -> Maybe Desviacion
desvStd [] = Nothing
desvStd m =
    case varianza m of
        Nothing -> Nothing
        Just (Varianza v) ->
            Just $ Desviacion (sqrt v)

incertidumbreA :: Muestra -> Maybe Incertidumbre
incertidumbreA [] = Nothing
incertidumbreA m =
    case desvStd m of
        Nothing -> Nothing
        Just (Desviacion d) ->
            let n = sqrt $ fromInteger (longitud m)
            in Just $ Incertidumbre (d / n)

-- Valor

promedioV :: [Valor] -> Maybe Promedio
promedioV [] = Nothing
{-
promedioV m =
    let l = fromInteger $ longitud m
        s = sumatoriaV m
    in Just $ Promedio (divValores s (Valor l))
-}
