
module Backend exposing(..)
import Models exposing(Movie, Preferences)
import Regex
import List exposing(member)


type HowMany
    = All
    | AtMost Int

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo! LISTO
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg" LISTO
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron" LISTO

toSentenceCase : String -> String
toSentenceCase word =
    String.toUpper word

peliculaTienePalabrasClave : String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula= List.all ((flip tienePalabraClave) pelicula) (separarEnPalabras palabras)

tienePalabraClave : String -> Movie -> Bool
tienePalabraClave palabra pelicula = String.contains (toSentenceCase palabra) (toSentenceCase pelicula.title)

separarEnPalabras : String -> List String
separarEnPalabras = Regex.split Regex.All (Regex.regex " ")

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector; LISTO
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (esDelGenero genero)

esDelGenero : String -> Movie -> Bool
esDelGenero genero pelicula = List.member (toSentenceCase genero) (List.map toSentenceCase pelicula.genre)

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox; LISTO
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = List.filter esAptaParaMenores

esAptaParaMenores : Movie -> Bool
esAptaParaMenores pelicula = pelicula.forKids

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.reverse << List.sortBy .rating

-- **************
-- Requerimiento: dar like a una película FUNCIONA, PERO SE PUEDE DAR MAS DE UN LIKE
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = List.map (peliculaADarleLike id)

peliculaADarleLike id pelicula =
  if pelicula.id == id then {pelicula | likes = pelicula.likes + 1}
  else pelicula



-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = List.map (calculoDePorcentajes preferencias)

calculoDePorcentajes : Preferences -> Movie -> Movie
calculoDePorcentajes preferencias = noSupere100 << sumarGenero preferencias.genre << sumarActorActriz preferencias.favoriteActor << sumarPalabrasClave preferencias.keywords
--calculoDePorcentajes preferencias = noSupere100 << sumarGenerosRecomendados preferencias.genre << sumarGenero preferencias.genre << sumarActorActriz preferencias.favoriteActor << sumarPalabrasClave preferencias.keywords


sumarPalabrasClave : String -> Movie -> Movie
sumarPalabrasClave palabras pelicula = {pelicula | matchPercentage = pelicula.matchPercentage + 20 * cantidadDePalabrasClaveQueContiene palabras pelicula}

cantidadDePalabrasClaveQueContiene : String -> Movie -> Int
cantidadDePalabrasClaveQueContiene palabras pelicula = List.length (List.filter ((flip tienePalabraClave) pelicula) (separarEnPalabras palabras))
{-sumarPalabrasClave [palabraClave :: demasPalabrasClave] pelicula = 
   case if tienePalabraClave palabraClave pelicula then
        pelicula.matchPercentage == pelicula.matchPercentage + 20
        sumarPalabrasClave demasPalabrasClave pelicula
    else 
        sumarPalabrasClave demasPalabrasClave pelicula

sumarPalabrasClave palabraClave pelicula =
    if tienePalabraClave palabraClave pelicula then
        pelicula.matchPercentage == pelicula.matchPercentage + 20
    else pelicula.matchPercentage-}

sumarActorActriz : String -> Movie -> Movie
sumarActorActriz actorFavorito pelicula =
    if tieneActorFavorito actorFavorito pelicula then
        {pelicula | matchPercentage = pelicula.matchPercentage + 50}
    else
        pelicula

tieneActorFavorito : String -> Movie -> Bool
tieneActorFavorito actorFavorito pelicula = List.member (toSentenceCase actorFavorito) (List.map toSentenceCase pelicula.actors)

sumarGenero : String -> Movie -> Movie
sumarGenero generoFavorito pelicula = 
    if esDelGenero generoFavorito pelicula then
        {pelicula | matchPercentage = pelicula.matchPercentage + 60}
    else 
        pelicula

--sumarGenerosRecomendados : String -> Movie -> Movie
--sumarGenerosRecomendados generoFavorito pelicula = 
--    if generoFavorito

noSupere100 : Movie -> Movie
noSupere100 pelicula =
    if pelicula.matchPercentage > 100 then
        {pelicula | matchPercentage = 100}
    else
        pelicula