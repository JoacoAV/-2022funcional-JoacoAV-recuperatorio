module Library where
import PdePreludat

data Postre=Postre{
    sabores::[String],
    peso::Number,
    temperatura::Number
}deriving (Show, Eq)

chocolate::String
chocolate = "chocolate"

frutilla::String
frutilla = "frutilla"

concentrado::String
concentrado = "concentrado"

bizcocho::Postre
bizcocho = (Postre [chocolate] 100 25)

type Hechizo = Postre -> Postre

incendio::Hechizo
incendio postre = Postre (sabores postre) (0.95 * peso postre) (1+temperatura postre)

immobulus::Hechizo
immobulus postre = Postre (sabores postre) (peso postre) (0*temperatura postre)

wingardiumLeviosa::Hechizo
wingardiumLeviosa postre = Postre ((sabores postre) ++ [concentrado]) (0.9*peso postre) (temperatura postre)

diffindo::Number -> Hechizo
diffindo porcent postre = Postre (sabores postre) (porcent * (peso postre) /100) (temperatura postre)

riddikulus::String -> Hechizo
riddikulus sabor postre = Postre ((sabores postre) ++ [reverse sabor]) (peso postre) (temperatura postre)

avadakedavra::Hechizo
avadakedavra postre = immobulus (postre{sabores = []})
