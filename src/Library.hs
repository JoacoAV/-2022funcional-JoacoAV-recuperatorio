module Library where
import PdePreludat

--------------------------------------------------------------------------------------------------------------
--A)

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
bizcocho = Postre [chocolate] 100 25

bizcochoCongelado::Postre
bizcochoCongelado = Postre [chocolate] 120 0

rosquilla::Postre
rosquilla = Postre [frutilla] 50 15

--------------------------------------------------------------------------------------------------------------
--B)

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

--------------------------------------------------------------------------------------------------------------
--C)

type Mesa = [Postre]
mesaDulce::Mesa
mesaDulce= [bizcocho, rosquilla]

muffinFantasma::Postre
muffinFantasma = Postre [] 0 30

tieneSabores::Postre -> Bool
tieneSabores postre = (sabores postre /= []) 

listo::Postre -> Bool
listo postre = (tieneSabores postre) && (peso postre > 0) &&  (temperatura postre > 0)

loDejaListo::Mesa -> Hechizo -> Bool
loDejaListo mesa hechizo = all listo (map hechizo mesa)

--------------------------------------------------------------------------------------------------------------
--D)

mesaFria::Mesa
mesaFria= [bizcocho, bizcochoCongelado, rosquilla]

cualesEstanListos::Mesa -> Mesa
cualesEstanListos mesa = filter listo mesa

pesoPromedioPostresListos::Mesa -> Number
pesoPromedioPostresListos mesa = (sum (map peso (cualesEstanListos mesa))) / (length (cualesEstanListos mesa)) 
--Tira error si no hay ningun postre listo, porque divide por cero

--------------------------------------------------------------------------------------------------------------
--E)
--Según el libro de cocina de Nicolas Flamel, la legendaria Mesa Filosofal tiene la capacidad de multiplicar cualquier postre que se coloque sobre ella.
mesaFilosofal:: Postre -> Mesa
mesaFilosofal postre = postre : mesaFilosofal postre

--Si. Por ejemplo si en una consulta de loDejaListo aplico los hechizos Immobulus o Avadakedavra esos hechizos congelan a los postres, y como el primer postre que se lee no está listo por estar congelado, la ejecución corta devolviendo False.
--Incendio deja a los postres calientes, asi que para que la ejecución corte requiere que los postres no tengan ningún sabor o tengan un peso igual a cero.
--Wingardium Leviosa le da un sabor y reduce un porcentaje del peso, asi que la ejecución nunca cortaría si los postres no tienen un peso o temperatura iguales a cero.
--Diffindo podría cortar la ejecución con cualquier postre, pero solo si se le indica un "0" como porcentaje
--Riddikulus solo agrega un sabor, asi que nunca podría devolver un resultado a no ser que el peso o temperatura de los postres sea cero.
--En conclusión, si un hechizo no deja el peso en 0, la temperatura en 0 o no deja al postre sin sabores, la ejecución nunca cortaría porque todos los postres que lee están listos y nunca va a encontrar uno no listo. 