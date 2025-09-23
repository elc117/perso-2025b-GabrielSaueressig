{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser, Object)
import qualified Data.Text as T
import Network.HTTP.Req
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Data.List (nub, sortOn, nubBy)
import Data.Maybe (fromMaybe)
import qualified Data.Aeson.KeyMap as KM
import System.Random (randomRIO)
import Data.Ord (Down(..))


-- API Key do TMDB
tmdbApiKey :: T.Text
tmdbApiKey = "062ee31d45a5eee7e0386f738a396a18"

data Generos = Generos
  { favoritos :: [String]  -- IMDB IDs dos filmes favoritos
  , generos   :: [String]  -- Gêneros escolhidos pelo usuário
  } deriving (Show, Generic)

instance FromJSON Generos
instance ToJSON Generos

-- Novo tipo Movie para incluir IMDB ID (vem do TMDB)
data Movie = Movie
  { title      :: T.Text
  , releaseDate :: T.Text
  , voteAverage :: Double
  , genres :: [T.Text]
  , poster :: T.Text
  , genreIds :: [Int]
  , imdbId :: Maybe T.Text  -- IMDB ID do TMDB
  } deriving (Show, Generic)

instance ToJSON Movie where
  toJSON Movie{..} = object
    [ "title" .= title
    , "releaseDate" .= releaseDate  
    , "voteAverage" .= voteAverage
    , "genres" .= genres
    , "poster" .= poster
    , "imdbId" .= imdbId
    ]

instance FromJSON Movie where
  parseJSON = withObject "Movie" $ \o -> do
    title       <- o .: "title"
    releaseDate <- o .: "release_date"
    voteAverage <- o .: "vote_average"
    genreIds    <- o .: "genre_ids"
    posterPath  <- o .:? "poster_path" .!= ""
    imdbId      <- o .:? "imdb_id"  -- Pode não vir na busca
    let genres = map tmdbIdToGenre genreIds
        poster = construirUrlPoster posterPath
    return Movie {..}

data DiscoverResp = DiscoverResp
  { results :: [Movie]
  } deriving Show

instance FromJSON DiscoverResp where
    parseJSON = withObject "DiscoverResp" $ \o ->
        DiscoverResp <$> o .: "results"

-- Resposta do endpoint /find (para buscar por IMDB ID)
data FindResp = FindResp
  { movieResults :: [Movie]
  } deriving Show

instance FromJSON FindResp where
    parseJSON = withObject "FindResp" $ \o ->
        FindResp <$> o .: "movie_results"

-- Construir URL completa do poster TMDB
construirUrlPoster :: T.Text -> T.Text
construirUrlPoster posterPath = 
    if T.null posterPath 
    then "https://placehold.co/300x450/cccccc/666666?text=No+Poster"
    else "https://image.tmdb.org/t/p/w500" <> posterPath

-- Política de CORS
policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy
           { corsRequestHeaders = ["Content-Type"]
           , corsMethods = ["GET","POST","OPTIONS"]
           }

-- Mapear IDs do TMDB para nomes de gêneros
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

-- Mapear nomes de gêneros para IDs do TMDB
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

-- Buscar filme por IMDB ID usando TMDB
buscarFilmePorImdbId :: T.Text -> IO (Maybe Movie)
buscarFilmePorImdbId imdbId = runReq defaultHttpConfig $ do
    let opts = "api_key" =: tmdbApiKey <>
               "external_source" =: ("imdb_id" :: T.Text)
    
    r <- req GET (https "api.themoviedb.org" /: "3" /: "find" /: imdbId)
             NoReqBody jsonResponse opts
    
    let body = responseBody r :: FindResp
        filmes = movieResults body
    
    return $ case filmes of
        (filme:_) -> Just filme
        [] -> Nothing

-- Buscar gêneros dos filmes favoritos usando TMDB
buscaGenerosFavoritos :: [String] -> IO [[T.Text]]
buscaGenerosFavoritos imdbIds = do
    filmes <- mapM (buscarFilmePorImdbId . T.pack) imdbIds
    return $ map (maybe [] genres) filmes

-- gerar recomendadados
getFilmesRecomendados :: [T.Text] -> IO [Movie]
getFilmesRecomendados generosPreferidos = do
    let genreIds = filter (not . T.null) $ map genreToTMDBId generosPreferidos
    
    if null genreIds
    then getFilmesRecentes
    else do

        filmesAmplos <- buscarFilmesAmplos genreIds
        
        -- Ordena por score de compatibilidade
        let filmesComScore = map (\filme -> (filme, pontuarFilme generosPreferidos filme)) filmesAmplos
            filmesOrdenados = map fst $ sortOn (Down . snd) filmesComScore
            melhoresFilmes = take 60 filmesOrdenados
        
        return melhoresFilmes

-- Busca ampla usando OR
buscarFilmesAmplos :: [T.Text] -> IO [Movie]
buscarFilmesAmplos genreIds = do
    -- junta cinco generos de cada vez, ja que tmdb so aceita 5
    let chunksGeneros = chunksOf 5 genreIds 
    
    todosFilmes <- mapM buscarChunkGeneros chunksGeneros
    
    -- Remove duplicatas por título
    let filmesUnicos = nubBy (\a b -> title a == title b) (concat todosFilmes)
    
    return filmesUnicos

-- Divide lista em chunks de tamanho específico
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Busca um grupo de gêneros usando OR
buscarChunkGeneros :: [T.Text] -> IO [Movie]
buscarChunkGeneros genreIds = runReq defaultHttpConfig $ do
    let genreString = T.intercalate "|" genreIds  -- | = OR no TMDB
        opts = "api_key" =: tmdbApiKey <>
               "with_genres" =: genreString <>
               "sort_by" =: ("popularity.desc" :: T.Text) <>
               "page" =: (1 :: Int) <>
               "vote_average.gte" =: ("6.0" :: T.Text) <>
               "language" =: ("en-US" :: T.Text)
    
    liftIO $ putStrLn $ "Buscando gêneros: " ++ T.unpack genreString
    
    r <- req GET (https "api.themoviedb.org" /: "3" /: "discover" /: "movie")
                 NoReqBody jsonResponse opts
    
    let body = responseBody r :: DiscoverResp
        resultados = results body
    
    pure resultados

-- Buscar filmes de várias páginas aleatórias
getFilmesRecentes :: IO [Movie]
getFilmesRecentes = do
    pagina1 <- randomRIO (1, 5) 
    pagina2 <- randomRIO (6, 15)   
    pagina3 <- randomRIO (16, 25)
    pagina4 <- randomRIO (26, 35) 
    
    filmes1 <- buscarPorCritério "popularity.desc" pagina1
    filmes2 <- buscarPorCritério "popularity.desc" pagina2
    filmes3 <- buscarPorCritério "popularity.desc" pagina3
    filmes4 <- buscarPorCritério "popularity.desc" pagina4
    
    return (filmes1 ++ filmes2 ++ filmes3 ++ filmes4)

-- Buscar por critério geral
buscarPorCritério :: T.Text -> Int -> IO [Movie]
buscarPorCritério sortBy pageNum = runReq defaultHttpConfig $ do
    let opts =
          "api_key"        =: tmdbApiKey <>
          "sort_by"        =: sortBy <>
          "primary_release_date.gte" =: ("1995-01-01" :: T.Text) <>
          "vote_average.gte" =: ("7.0" :: T.Text) <>
          "page"           =: pageNum <>
          "language"       =: ("en-US" :: T.Text)
    
    r <- req GET (https "api.themoviedb.org" /: "3" /: "discover" /: "movie")
                 NoReqBody jsonResponse opts
    let body = responseBody r :: DiscoverResp
    pure (results body)

-- Estrutura para resposta de busca do TMDB
data SearchResp = SearchResp
  { searchResults :: [Movie]
  } deriving Show

instance FromJSON SearchResp where
    parseJSON = withObject "SearchResp" $ \o ->
        SearchResp <$> o .: "results"

-- Buscar filmes por título usando TMDB
buscarFilmesPorTitulo :: T.Text -> IO [Movie]
buscarFilmesPorTitulo titulo = runReq defaultHttpConfig $ do
    let opts = "api_key" =: tmdbApiKey <>
               "query" =: titulo <>
               "language" =: ("en-US" :: T.Text)
    
    r <- req GET (https "api.themoviedb.org" /: "3" /: "search" /: "movie")
             NoReqBody jsonResponse opts
    
    let body = responseBody r :: SearchResp
    return (searchResults body)

-- Pontuação
pontuarFilme :: [T.Text] -> Movie -> Double
pontuarFilme generosPreferidos filme =
    let generosFilme = genres filme
        matches = filter (`elem` generosPreferidos) generosFilme
        numMatches = length matches
        totalPreferidos = length generosPreferidos
        
        -- Porcentagem de compatibilidade
        compatibilidade = fromIntegral numMatches / fromIntegral totalPreferidos
        
        scoreGeneros = compatibilidade ** 1.5
        
        -- Bonus extra para alta compatibilidade
        bonusCompatibilidade = if compatibilidade >= 0.5  -- 50% ou mais dos gêneros
                              then compatibilidade * 3.0
                              else 0.0
        
        -- Penalidade para filmes sem nenhum gênero preferido
        penalidade = if numMatches == 0 then -20.0 else 0.0
        
        scoreTotal = scoreGeneros + bonusCompatibilidade + penalidade
        
    in scoreTotal

-- Junta gêneros escolhidos com favoritos e remove duplicados
geraGenerosTotais :: Generos -> IO [T.Text]
geraGenerosTotais g = do
    let escolhidos = map T.pack (generos g)
    generosFav <- buscaGenerosFavoritos (favoritos g)
    return $ nub (escolhidos ++ concat generosFav)

-- Servidor
main :: IO ()
main = scotty 3000 $ do
    middleware $ cors (const $ Just policy)

    -- Endpoint /hello
    get "/hello" $
        json $ object ["message" .= ("Hello, backend em Haskell com TMDB!" :: T.Text)]

    -- Endpoint para buscar filmes por título 
    get "/search/movie" $ do
        titulo <- Web.Scotty.queryParam "title"
        filmes <- liftIO $ buscarFilmesPorTitulo titulo
        json filmes

    -- Endpoint /generos - Sistema de recomendação melhorado
    post "/recommend/genero/recentes" $ do
        genero <- jsonData :: ActionM Generos
        generosTotais <- liftIO $ geraGenerosTotais genero
        liftIO $ print ("Gêneros totais:", generosTotais)
        
        -- Buscar filmes usando filtros específicos de gênero
        filmesRecomendados <- liftIO $ getFilmesRecomendados generosTotais
        
        -- Calcular score melhorado e ordenar
        let filmesComScore = map (\filme -> (filme, pontuarFilme generosTotais filme)) filmesRecomendados
            filmesOrdenados = map fst $ sortOn (Down . snd) filmesComScore
            filmesFinal = take 20 filmesOrdenados
        
        json filmesFinal