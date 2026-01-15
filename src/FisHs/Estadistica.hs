module FisHs.Estadistica where
import FisHs.Core
import FisHs.Types

type Muestra = [Double]

newtype Promedio = Promedio Double deriving Show
newtype Varianza = Varianza Double deriving Show
newtype Desviacion = Desviacion Double deriving Show

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

-- Valor

promedioV :: [Valor] -> Maybe Promedio
promedioV [] = Nothing
