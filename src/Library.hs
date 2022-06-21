module Library where
import PdePreludat

data Postre=Postre{
    sabores::[String],
    peso::Number,
    temperatura::Number
}deriving (Show, Eq)
