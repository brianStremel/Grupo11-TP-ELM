
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
tienePalabraClave palabras pelicula = String.contains (toSentenceCase palabras) (toSentenceCase pelicula.title)

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
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = completaAca

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = completaAca
