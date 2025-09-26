{-# LANGUAGE OverloadedStrings #-}
module TestMain where
import Main 
import Test.HUnit
import qualified Data.Text as T
import Data.List (nub)

-- Lista principal de testes
testCases :: Test
testCases = TestList
    [ TestLabel "construirUrlPoster" testConstruirUrlPoster
    , TestLabel "tmdbIdToGenre" testTmdbIdToGenre
    , TestLabel "genreToTMDBId" testGenreToTMDBId
    , TestLabel "chunksOf" testChunksOf
    , TestLabel "pontuarFilme" testPontuarFilme
    , TestLabel "buscarFilmePorImdbId" testBuscarFilmePorImdbId
    , TestLabel "buscaGenerosFavoritos" testBuscaGenerosFavoritos
    , TestLabel "buscarGenerosPorId" testBuscarGenerosPorId
    , TestLabel "getFilmesRecomendados" testGetFilmesRecomendados
    , TestLabel "geraGenerosTotais" testGeraGenerosTotais
    ]

-- Teste 1: construirUrlPoster
testConstruirUrlPoster :: Test
testConstruirUrlPoster = TestCase $ do
    assertEqual "Teste 1 para construir url do poster de exibição" 
        "https://image.tmdb.org/t/p/w500filmePath" 
        (construirUrlPoster "filmePath")
    assertEqual "Teste 2 para construir url sem imagem de poster" 
        "https://placehold.co/300x450/cccccc/666666?text=No+Poster" 
        (construirUrlPoster "")

-- Teste 2: tmdbIdToGenre
testTmdbIdToGenre :: Test
testTmdbIdToGenre = TestCase $ do
    assertEqual "Teste 1 para tmdbId para generos" 
        "Action" 
        (tmdbIdToGenre 28)
    assertEqual "Teste 2 ID inválido" 
        "Unknown" 
        (tmdbIdToGenre 99999)
    assertEqual "Teste 3 Horror" 
        "Horror" 
        (tmdbIdToGenre 27)
    assertEqual "Teste 4 Romance" 
        "Romance" 
        (tmdbIdToGenre 10749)

-- Teste 3: genreToTMDBId  
testGenreToTMDBId :: Test
testGenreToTMDBId = TestCase $ do
    assertEqual "Teste 1 para genero action para tmdbId" 
        "28" 
        (genreToTMDBId "action")
    assertEqual "Teste 2 para genero horror para tmdbId" 
        "27" 
        (genreToTMDBId "horror")
    assertEqual "Teste 3 para genero romance para tmdbId" 
        "10749" 
        (genreToTMDBId "romance")
    assertEqual "Teste 4 genero maiúsculo" 
        "28" 
        (genreToTMDBId "ACTION")
    assertEqual "Teste 5 genero inválido" 
        "" 
        (genreToTMDBId "invalidgenre")

-- Teste 4: chunksOf
testChunksOf :: Test
testChunksOf = TestCase $ do
    assertEqual "Teste 1 para separação em chunks de 2" 
        [[1,2],[3,4],[5]] 
        (chunksOf 2 [1,2,3,4,5])
    assertEqual "Teste 2 lista vazia" 
        ([] :: [[Int]]) 
        (chunksOf 2 [])
    assertEqual "Teste 3 chunk maior que lista" 
        [[1,2,3]] 
        (chunksOf 5 [1,2,3])
    assertEqual "Teste 4 chunks de 1" 
        [[1],[2],[3],[4]] 
        (chunksOf 1 [1,2,3,4])

-- Teste 5: pontuarFilme
testPontuarFilme :: Test
testPontuarFilme = TestCase $ do
    let filme1 = Movie "Test Movie" "2023-01-01" 8.0 ["Action", "Adventure"] "" [28,12] Nothing
    let generosPreferidos = ["Action", "Adventure"]
    let score = pontuarFilme generosPreferidos filme1
    assertBool "Teste 1 Score deve ser > 10 para match completo" (score > 10.0)

    let filme2 = Movie "Test Movie 2" "2023-01-01" 8.0 ["Comedy"] "" [35] Nothing
    let score2 = pontuarFilme generosPreferidos filme2
    assertEqual "Teste 2 Score deve ser 0 para nenhum match" 0.0 score2

    let filme3 = Movie "Test Movie 3" "2023-01-01" 9.0 ["Action", "Adventure", "Comedy"] "" [28,12,35] Nothing
    let filme4 = Movie "Test Movie 4" "2023-01-01" 9.0 ["Action"] "" [28] Nothing
    let score3 = pontuarFilme generosPreferidos filme3
    let score4 = pontuarFilme generosPreferidos filme4
    assertBool "Teste 3 Filme com mais matches deve ter score maior" (score3 > score4)

    let filmeAlta = Movie "High Rated" "2023-01-01" 9.0 ["Action"] "" [28] Nothing
    let filmeBaixa = Movie "Low Rated" "2023-01-01" 5.0 ["Action"] "" [28] Nothing
    let scoreAlta = pontuarFilme ["Action"] filmeAlta
    let scoreBaixa = pontuarFilme ["Action"] filmeBaixa
    assertBool "Teste 4 Filme com nota maior deve ter score maior" (scoreAlta > scoreBaixa)

-- Teste 6: buscarFilmePorImdbId
testBuscarFilmePorImdbId :: Test
testBuscarFilmePorImdbId = TestCase $ do
    filme <- buscarFilmePorImdbId "tt0111161"
    case filme of
      Just f -> do
        assertBool "Teste 1 Título deve conter 'Shawshank'" ("Shawshank" `T.isInfixOf` title f)
      Nothing -> assertFailure "Filme não encontrado via IMDB ID"

-- Teste 7: buscaGenerosFavoritos
testBuscaGenerosFavoritos :: Test
testBuscaGenerosFavoritos = TestCase $ do
    generos <- buscaGenerosFavoritos ["tt0111161", "Your Name_2016-08-26"]
    assertBool "Deve retornar lista com 2 elementos" (length generos == 2)
    let todosGeneros = concat generos
    assertBool "Deve conter 'Drama'" ("Drama" `elem` map T.unpack todosGeneros)

-- Teste 8: buscarGenerosPorId
testBuscarGenerosPorId :: Test
testBuscarGenerosPorId = TestCase $ do
    g1 <- buscarGenerosPorId "tt0111161"
    g2 <- buscarGenerosPorId "Your Name_2016-08-26"
    assertBool "Teste 1 IMDB ID deve ter 'Drama'" ("Drama" `elem` map T.unpack g1)
    assertBool "Teste 2 Título_data deve ter 'Animation'" ("Animation" `elem` map T.unpack g2)

-- Teste 9: getFilmesRecomendados
testGetFilmesRecomendados :: Test
testGetFilmesRecomendados = TestCase $ do
    filmes <- getFilmesRecomendados ["Horror", "Romance", "Animation"]
    assertBool "Deve retornar ao menos 1 filme" (not $ null filmes)
    let titulos = map title filmes
    assertBool "Todos os filmes devem ter título" (all (not . T.null) titulos)


testGeraGenerosTotais :: Test
testGeraGenerosTotais = TestCase $ do
    let input = Generos
          { favoritos = ["tt5164214"] -- FIlme Ocean’s Eight
          , generos   = ["Horror","Romance"]
          }
    resultado <- geraGenerosTotais input  
    let resultadoStr = map T.unpack resultado
    assertEqual "Teste 1 Deve Juntar Gêneros" ["Horror","Romance","Crime","Comedy","Action"] resultadoStr

-- Função principal
main :: IO ()
main = do

    resultado <- runTestTT testCases
    
    let totalTestes = cases resultado
        falhas = failures resultado
        erros = errors resultado
        sucessos = totalTestes - falhas - erros
    
    putStrLn $ "Sucessos: " ++ show sucessos
    putStrLn $ "Falhas: " ++ show falhas  
    putStrLn $ "Erros: " ++ show erros
    putStrLn $ "Total: " ++ show totalTestes
    
    if falhas == 0 && erros == 0
    then putStrLn "Todos os testes passaram!"
    else putStrLn "Alguns testes falharam"
    
    return ()