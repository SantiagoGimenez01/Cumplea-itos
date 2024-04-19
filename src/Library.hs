module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Invitado = UnInvitado{
    cansancio :: Number,
    felicidad :: Number,
    cancionFavorita :: String
} deriving(Show, Eq)

estaCansado :: Invitado -> Bool
estaCansado invitado = cansancio invitado > 80

seCansa :: Invitado -> Invitado
seCansa invitado = UnInvitado{felicidad = felicidad invitado, cansancio = cansancio invitado + 10, cancionFavorita = cancionFavorita invitado }

disfruta :: Invitado -> Invitado
disfruta invitado = UnInvitado{felicidad = felicidad invitado + 100 - cansancio invitado, cansancio = cansancio invitado, cancionFavorita = cancionFavorita invitado}

santi :: Invitado
santi = UnInvitado{cansancio = 75, felicidad = 80, cancionFavorita = "Besame remix"}
