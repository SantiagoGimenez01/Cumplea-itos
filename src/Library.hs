module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Invitado = UnInvitado{
    cansancio :: Number,
    felicidad :: Number,
    cancionFavorita :: String
} deriving(Show, Eq)

type Plancito = Invitado -> Invitado

type Bandurria = [Invitado]

estaCansado :: Invitado -> Bool
estaCansado invitado = cansancio invitado > 80

seCansa :: Invitado -> Invitado
seCansa invitado = UnInvitado{felicidad = felicidad invitado, cansancio = cansancio invitado + 10, cancionFavorita = cancionFavorita invitado }

disfruta :: Invitado -> Invitado
disfruta invitado = UnInvitado{felicidad = felicidad invitado + 100 - cansancio invitado, cansancio = cansancio invitado, cancionFavorita = cancionFavorita invitado}

santi :: Invitado
santi = UnInvitado{cansancio = 75, felicidad = 80, cancionFavorita = "Besame remix"}

mili :: Invitado
mili = UnInvitado{cansancio = 60, felicidad = 90, cancionFavorita = "Don't Blame Me"}

male :: Invitado
male = UnInvitado{cansancio = 82, felicidad = 100, cancionFavorita = "MP3"}

paula :: Invitado
paula = UnInvitado{cansancio = 85, felicidad = 100, cancionFavorita = "La Original"}


unaCharlitaDeFulbo :: Plancito
unaCharlitaDeFulbo = disfruta

bailar :: Plancito
bailar = seCansa.disfruta

mesaDulce :: Plancito
mesaDulce  = disfruta.seCansa

tieneBuenGusto :: Invitado -> Bool 
tieneBuenGusto = even.length.cancionFavorita

leVaADarFiaca :: Plancito -> Invitado -> Bool
leVaADarFiaca plancito invitado = estaCansado(plancito invitado) 

playlistDeBandurria :: Bandurria -> [String]
playlistDeBandurria bandurria = map cancionFavorita bandurria

sigueLaGira :: Bandurria -> Bandurria
sigueLaGira bandurria = filter (not.estaCansado) bandurria 

estadoDeBandurria:: Bandurria -> Plancito -> Bandurria
estadoDeBandurria invitados plancito = map plancito invitados

laRompe :: Bandurria -> Bool
laRompe bandurria = all (not.estaCansado) bandurria

suenaUnHitazo :: Bandurria -> Bool
suenaUnHitazo bandurria = any tieneBuenGusto bandurria

seArmaFieston :: Bandurria -> Bool
seArmaFieston bandurria = sum (map felicidad bandurria) > 300

seLaSubeABandurria :: Bandurria -> Plancito -> Bool
seLaSubeABandurria bandurria plancito = seArmaFieston(estadoDeBandurria bandurria plancito)