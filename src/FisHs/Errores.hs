module FisHs.Errores where
import FisHs.Core
import FisHs.Types
import FisHs.Estadistica

newtype ErrorAbs = ErrorAbs
    {getErrorAbs :: Valor} deriving (Show, Eq)
newtype ErrorRel = ErrorRel
    {getErrorRel :: Valor} deriving (Show, Eq)

errorAbs :: Muestra -> Valor -> Either ErrorFis ErrorAbs
errorAbs (Muestra []) _ = Left MuestraVacia
errorAbs (Muestra m) (Valor esperado) = do
    Promedio p <- promedio (Muestra m)
    let dif = restaV p (Valor esperado) -- restaV no da Maybe Valor
    return $ ErrorAbs (absV dif)

errorRel :: Muestra -> Valor -> Either ErrorFis ErrorRel
errorRel (Muestra []) _ = Left MuestraVacia
errorRel (Muestra m) (Valor esperado) = do
    ErrorAbs eAbs <- errorAbs (Muestra m) (Valor esperado)
    v <- divV eAbs (Valor esperado)
    return $ ErrorRel v
