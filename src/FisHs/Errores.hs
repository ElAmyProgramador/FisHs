module FisHs.Errores where
import FisHs.Core
import FisHs.Types
import FisHs.Estadistica

newtype ErrorAbs = ErrorAbs Valor deriving Show
newtype ErrorRel = ErrorRel Valor deriving Show

errorAbs :: Muestra -> Valor -> Maybe ErrorAbs
errorAbs (Muestra []) _ = Nothing
errorAbs (Muestra m) (Valor esperado) =
    case promedio (Muestra m) of
        Nothing -> Nothing
        Just (Promedio p) ->
            let dif = restaValores p (Valor esperado)
            in Just $ ErrorAbs (absV dif)

errorRel :: Muestra -> Valor -> Maybe ErrorRel
errorRel (Muestra []) _ = Nothing
errorRel (Muestra m) (Valor esperado) =
    case errorAbs (Muestra m) (Valor esperado) of
        Nothing -> Nothing
        Just (ErrorAbs valorA) ->
            ErrorRel <$> divValores (absV valorA) (Valor esperado)
