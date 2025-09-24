module Main where

-- import backend
import backend.hs
import Test.HUnit
import qualified Data.Text as T
import Data.List (nub)

tmdbIdToGenre :: Int -> T.Text
tmdbIdToGenre genreId = case genreId of
    28 -> "Action"
    12 -> "Adventure"
    16 -> "Animation"
    35 -> "Comedy"
    80 -> "Crime"
    99 -> "Documentary"
    18 -> "Drama"
    10751 -> "Family"
    14 -> "Fantasy"
    36 -> "History"
    27 -> "Horror"
    10402 -> "Music"
    9648 -> "Mystery"
    10749 -> "Romance"
    878 -> "Science Fiction"
    10770 -> "TV Movie"
    53 -> "Thriller"
    10752 -> "War"
    37 -> "Western"
    _ -> "Unknown"

genreToTMDBId :: T.Text -> T.Text
genreToTMDBId genre = case T.toLower genre of
    "action" -> "28"
    "adventure" -> "12"
    "animation" -> "16"
    "comedy" -> "35"
    "crime" -> "80"
    "documentary" -> "99"
    "drama" -> "18"
    "family" -> "10751"
    "fantasy" -> "14"
    "history" -> "36"
    "horror" -> "27"
    "music" -> "10402"
    "mystery" -> "9648"
    "romance" -> "10749"
    "science fiction" -> "878"
    "sci-fi" -> "878"
    "tv movie" -> "10770"
    "thriller" -> "53"
    "war" -> "10752"
    "western" -> "37"
    _ -> ""

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


testCases :: Test
testCases =
  TestList
    [ 
        construirUrlPoster,
        tmdbIdToGenre,
        genreToTMDBId,
        buscarFilmePorImdbId,
        buscaGenerosFavoritos,
        buscarGenerosPorId,
        getFilmesRecomendados,
        buscarFilmesAmplos,
        chunksOf,
        pontuarFilme,
        geraGenerosTotais
    ]

testConstruirUrlPoster :: Test
testConstruirUrlPoster = TestCase $ do
    asserEqual "Teste 1 para construir url do poster de exibição" ["https://image.tmdb.org/t/p/w500/filmePath"](construirUrlPoster "filmePath")
    asserEqual "Teste 2 para construir url sem imagem de poster" ["https://placehold.co/300x450/cccccc/666666?text=No+Poster"](construirUrlPoster "")

testTmdbIdToGenre :: Test
testTmdbIdToGenre = TestCase $ do
    asserEqual "Teste para tmdbId para generos" ["Action"] (tmdbIdToGenre 28)
    assertEqual "Teste 2 ID inválido" "Unknown" (tmdbIdToGenre 99999)

testGenreToTMDBId :: Test
testGenreToTMDBId = TestCase $ do
    asserEqual "Teste para genero para imdbId" [28](genreToTmdbId "action")

testChunckOf:: Test
testChunckOf = TestCase $ do
    asserEqual "Teste 1 para separação em chuncks de 2" [[1,2],[3,4],[5]] (chunksOf 2 [1,2,3,4,5])
    assertEqual "Teste 2 lista vazia" ([] :: [[Int]]) (chunksOf 2 [])
    assertEqual "Teste 3 chunk maior que lista" [[1,2,3]] (chunksOf 5 [1,2,3])

testPontuarFilme :: Test
testPontuarFilme = TestCase $ do
    let filme1 = Movie "Test Movie" ["Action", "Adventure"] 8.0
    let generosPreferidos = ["Action", "Adventure"]
    let score = pontuarFilme generosPreferidos filme1
    assertBool "Teste 1 Score deve ser > 10 para match completo" (score > 10.0)

    let filme2 = Movie "Test Movie 2" ["Comedy"] 8.0
    let generosPreferidos = ["Action", "Adventure"]
    let score = pontuarFilme generosPreferidos filme2
    assertEqual "Teste 2 Score deve ser 0 para nenhum match" 0.0 score

    let filme3 = Movie "Test Movie 3" ["Action", "Adventure", "Comedy"] 9.0
    let generosPreferidos = ["Action", "Adventure"]
    let score1 = pontuarFilme generosPreferidos filme3
    let filme4 = Movie "Test Movie 5" ["Action"] 9.0
    let score2 = pontuarFilme generosPreferidos filme4
    assertBool "Teste 3 Filme com mais matches deve ter score maior" (score1 > score2)

testBuscarFilmePorImdbId :: Test
testBuscarFilmePorImdbId = TestCase $ do
    let imdbIdValido = T.isPrefixOf "tt" "tt0111161"
    assertBool "Teste 1 IMDB ID deve começar com 'tt'" imdbIdValido

testBuscaGenerosFavoritos :: Test
testBuscaGenerosFavoritos = TestCase $ do
    let favoritos = ["tt0111161", "Your Name_2016-08-26", "tt0068646"]
    let imdbIds = filter (T.isPrefixOf "tt" . T.pack) favoritos
    let tituloData = filter (not . T.isPrefixOf "tt" . T.pack) favoritos
    
    assertEqual "Teste 1 Deve identificar 2 IMDB IDs" 2 (length imdbIds)
    assertEqual "Teste 2 Deve identificar 1 título_data" 1 (length tituloData)

testBuscarGenerosPorId :: Test  
testBuscarGenerosPorId = TestCase $ do
    let imdbId = "tt0111161"
    let tituloData = "Your Name_2016-08-26"
    
    assertBool "Teste 1 Deve detectar IMDB ID" (T.isPrefixOf "tt" (T.pack imdbId))
    assertBool "Teste 2 Não deve detectar título_data como IMDB" (not $ T.isPrefixOf "tt" (T.pack tituloData))
    
    let tituloExtraido = takeWhile (/= '_') tituloData
    assertEqual "Teste 3 Deve extrair título corretamente" "Your Name" tituloExtraido


testGetFilmesRecomendados :: Test
testGetFilmesRecomendados = TestCase $ do
    let generosPreferidos = ["Horror", "Romance", "Animation"]
    let genreIds = filter (not . T.null) $ map genreToTMDBId generosPreferidos
    
    assertEqual "Teste 1 Deve converter 3 gêneros" 3 (length genreIds)
    assertBool "Teste 2 Deve conter ID do Horror" ("27" `elem` genreIds)
    assertBool "Teste 3 Deve conter ID do Romance" ("10749" `elem` genreIds)
    assertBool "Teste 4 Deve conter ID da Animation" ("16" `elem` genreIds)

testBuscarFilmesAmplos :: Test
testBuscarFilmesAmplos = TestCase $ do

    let genreIds = ["27", "10749", "16", "35", "18", "28", "12"]
    let chunks = chunksOf 3 genreIds
    
    assertEqual "Deve criar 3 chunks" 3 (length chunks)
    assertEqual "Primeiro chunk deve ter 3 elementos" 3 (length $ head chunks)
    assertEqual "Último chunk deve ter 1 elemento" 1 (length $ last chunks)

testGeraGenerosTotais :: Test
testGeraGenerosTotais = TestCase $ do
    let escolhidos = ["Horror", "Romance"]
    let dosFavoritos = [["Animation", "Romance"], ["Drama", "Horror"]]
    let combinados = nub (escolhidos ++ concat dosFavoritos)
    
    assertEqual "Teste 1 Deve ter 4 gêneros únicos" 4 (length combinados)
    assertBool "Teste 2 Deve conter Horror" ("Horror" `elem` combinados)
    assertBool "Teste 3 Deve conter Romance" ("Romance" `elem` combinados)
    assertBool "Teste 4 Deve conter Animation" ("Animation" `elem` combinados)
    assertBool "Teste 5 Deve conter Drama" ("Drama" `elem` combinados)



main :: IO ()
main = do
  putStrLn "Rodando Testes..."
  runTestTT testCases
  return ()