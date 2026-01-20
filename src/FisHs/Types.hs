module FisHs.Types where

newtype Valor = Valor
    {unValor :: Double} deriving (Show, Eq, Ord)
newtype Segundo = Segundo Double
newtype Metro = Metro Double
newtype Kilogramo = Kilogramo Double
newtype Muestra = Muestra
    {valores :: [Valor]} deriving (Show, Eq)

data ErrorFis
    = MuestraVacia
    | DivisionSobreCero
    | ValorEsperadoCero
    | ValorInvalido String
    deriving (Show)
