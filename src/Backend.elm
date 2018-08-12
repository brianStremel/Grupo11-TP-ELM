
module Backend exposing(..)
import Models exposing(Movie, Preferences)
import Regex
import List exposing(member)


type HowMany
    = All
    | AtMost Int

completaAca = identity

-- FILTRAR PELICULAS POR TITULO

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

toSentenceCase : String -> String
toSentenceCase word =
    String.toUpper word

peliculaTienePalabrasClave : String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula= List.all ((flip tienePalabraClave) pelicula) (separarEnPalabras palabras)

tienePalabraClave : String -> Movie -> Bool
tienePalabraClave palabra pelicula = String.contains (toSentenceCase palabra) (toSentenceCase pelicula.title)

separarEnPalabras : String -> List String
separarEnPalabras = Regex.split Regex.All (Regex.regex " ")

-- FILTRAR PELICULAS POR GENERO

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (esDelGenero genero)

esDelGenero : String -> Movie -> Bool
esDelGenero genero pelicula = List.member (toSentenceCase genero) (List.map toSentenceCase pelicula.genre)

-- FILTRAR PELICULAS MENORES DE EDAD

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = List.filter esAptaParaMenores

esAptaParaMenores : Movie -> Bool
esAptaParaMenores pelicula = pelicula.forKids

-- ORDENAR PELICULAS POR RATING

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.reverse << List.sortBy .rating

-- DAR LIKE

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = List.map (peliculaADarleLike id)

peliculaADarleLike id pelicula =
  if pelicula.id == id then {pelicula | likes = pelicula.likes + 1}
  else pelicula



-- CALCULAR PORCENTAJE DE COINCIDENCIA

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = List.map (calculoDePorcentajes preferencias)

calculoDePorcentajes : Preferences -> Movie -> Movie
calculoDePorcentajes preferencias = sumarGenero preferencias.genre << sumarActorActriz preferencias.favoriteActor << sumarPalabrasClave preferencias.keywords

sumarPalabrasClave : String -> Movie -> Movie
sumarPalabrasClave palabras pelicula = sumarPorcentaje (20 * cantidadDePalabrasClaveQueContiene palabras pelicula) pelicula

cantidadDePalabrasClaveQueContiene : String -> Movie -> Int
cantidadDePalabrasClaveQueContiene palabras pelicula =
    if palabras /= "" then
      List.length (List.filter ((flip tienePalabraClave) pelicula) (separarEnPalabras palabras))
    else
      0


sumarActorActriz : String -> Movie -> Movie
sumarActorActriz actorFavorito pelicula =
    if tieneActorFavorito actorFavorito pelicula then
        sumarPorcentaje 50 pelicula
    else
        pelicula

tieneActorFavorito : String -> Movie -> Bool
tieneActorFavorito actorFavorito pelicula = List.member (toSentenceCase actorFavorito) (List.map toSentenceCase pelicula.actors)

sumarGenero : String -> Movie -> Movie
sumarGenero generoFavorito pelicula =
    if esDelGenero generoFavorito pelicula then
        sumarPorcentaje 60 pelicula
    else
        pelicula

sumarPorcentaje : Int -> Movie -> Movie -- SUMA PORCENTAJE Y EVITA QUE SE PASE DE 100
sumarPorcentaje porcentaje pelicula =
    if pelicula.matchPercentage + porcentaje <= 100 then
        {pelicula | matchPercentage = pelicula.matchPercentage + porcentaje}
    else
        {pelicula | matchPercentage = 100 }
