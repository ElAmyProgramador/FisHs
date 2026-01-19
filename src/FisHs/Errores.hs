module FisHs.Errores where
import FisHs.Core
import FisHs.Types
import FisHs.Estadistica

newtype ErrorAbs = ErrorAbs Valor deriving Show
newtype ErrorRel = ErrorRel Valor deriving Show

errorAbs :: Muestra -> Valor -> Maybe ErrorAbs
errorAbs (Muestra []) _ = Nothing
errorAbs (Muestra m) (Valor esperado) = do
    Promedio p <- promedio (Muestra m)
    let dif = restaV p (Valor esperado) -- retaV no da Maybe Valor
    return $ ErrorAbs (absV dif)

errorRel :: Muestra -> Valor -> Maybe ErrorRel
errorRel (Muestra []) _ = Nothing
errorRel (Muestra m) (Valor esperado) = do
    ErrorAbs eAbs <- errorAbs (Muestra m) (Valor esperado)
    v <- divV eAbs (Valor esperado)
    return $ ErrorRel v
