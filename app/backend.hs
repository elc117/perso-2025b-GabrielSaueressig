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
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Aeson.KeyMap as KM
import System.Random (randomRIO)
import Data.Ord (Down(..))

-- Função para buscar filmes
getFilmes :: String -> T.Text -> IO Value
getFilmes nome option = runReq defaultHttpConfig $ do
    let opts = option =: nome <> "apikey" =: ("ae725e23" :: String)
    r <- req GET (http "www.omdbapi.com") NoReqBody jsonResponse opts
    pure (responseBody r :: Value)

data Generos = Generos
  { favoritos :: [String]
  , generos   :: [String]
  } deriving (Show, Generic)

instance FromJSON Generos
instance ToJSON Generos

data Movie = Movie
  { title      :: T.Text
  , releaseDate :: T.Text
  , voteAverage :: Double
  , genres :: [T.Text]
  , poster :: T.Text
  , genreIds :: [Int]  -- IDs dos gêneros do TMDB
  }deriving (Show, Generic)

instance ToJSON Movie where
  toJSON Movie{..} = object
    [ "title" .= title
    , "releaseDate" .= releaseDate  
    , "voteAverage" .= voteAverage
    , "genres" .= genres
    , "poster" .= poster
    ]

-- Removendo o FromJSON personalizado já que não precisamos mais
instance FromJSON Movie where
  parseJSON = withObject "Movie" $ \o -> do
    title       <- o .: "title"
    releaseDate <- o .: "release_date"
    voteAverage <- o .: "vote_average"
    genreIds    <- o .: "genre_ids"  -- TMDB retorna array de IDs
    let genres = map tmdbIdToGenre genreIds  -- Converter IDs para nomes
        poster = ""
    return Movie {..}

data DiscoverResp = DiscoverResp
  { results :: [Movie]
  } deriving Show

instance FromJSON DiscoverResp where
    parseJSON = withObject "DiscoverResp" $ \o ->
        DiscoverResp <$> o .: "results"

-- Função para buscar poster via OMDB
buscarPoster :: T.Text -> IO T.Text
buscarPoster titulo = do
    filme <- getFilmes (T.unpack titulo) "t"
    case filme of
        Object o -> case KM.lookup "Poster" o of
            Just (String posterUrl) -> 
                if posterUrl == "N/A" 
                then return "https://placehold.co/300x450/cccccc/666666?text=No+Poster"
                else return posterUrl
            _ -> return "https://placehold.co/300x450/cccccc/666666?text=No+Poster"
        _ -> return "https://placehold.co/300x450/cccccc/666666?text=No+Poster"

-- Função para adicionar poster a um filme
adicionarPoster :: Movie -> IO Movie
adicionarPoster filme = do
    posterUrl <- buscarPoster (title filme)
    return filme { poster = posterUrl }

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

-- Função para buscar filmes de várias páginas aleatórias
getFilmesRecentes :: T.Text -> IO [Movie]
getFilmesRecentes apiKey = do
  -- Gerar 4 páginas aleatórias entre diferentes faixas
  pagina1 <- randomRIO (1, 5)   -- Páginas iniciais (mais populares)
  pagina2 <- randomRIO (6, 15)  -- Páginas médias  
  pagina3 <- randomRIO (16, 25) -- Páginas mais distantes
  pagina4 <- randomRIO (26, 35) -- Páginas bem distantes
  
  -- Buscar filmes de diferentes critérios também
  filmes1 <- buscarPorCritério apiKey "popularity.desc" pagina1
  filmes2 <- buscarPorCritério apiKey "vote_average.desc" pagina2  -- Por nota
  filmes3 <- buscarPorCritério apiKey "release_date.desc" pagina3  -- Mais recentes
  filmes4 <- buscarPorCritério apiKey "popularity.desc" pagina4
  
  return (filmes1 ++ filmes2 ++ filmes3 ++ filmes4)

buscarPorCritério :: T.Text -> T.Text -> Int -> IO [Movie]
buscarPorCritério apiKey sortBy pageNum = runReq defaultHttpConfig $ do
  let opts =
        "api_key"        =: apiKey <>
        "sort_by"        =: sortBy <>
        "primary_release_date.gte" =: ("2020-01-01" :: T.Text) <>
        "vote_average.gte" =: ("5.0" :: T.Text) <>
        "page"           =: pageNum <>
        "language"       =: ("en-US" :: T.Text)
  
  r <- req GET (https "api.themoviedb.org" /: "3" /: "discover" /: "movie")
               NoReqBody jsonResponse opts
  let body = responseBody r :: DiscoverResp
  pure (results body)

-- Mapear gêneros para IDs do TMDB
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

-- Calcula a "pontuação" de compatibilidade com os gêneros preferidos
pontuarFilme :: [T.Text] -> Movie -> Int
pontuarFilme generosPreferidos filme =
    let generosFilme = genres filme
        matches = filter (`elem` generosPreferidos) generosFilme
        -- Bonus por ter mais gêneros em comum
        scoreBase = length matches
        -- Bonus extra se tiver muitos gêneros em comum
        bonusMultiplo = if scoreBase >= 2 then scoreBase * 2 else scoreBase
    in bonusMultiplo

-- Extrai gêneros de um JSON de filme
extraiGeneros :: Value -> [T.Text]
extraiGeneros (Object o) =
    case KM.lookup "Genre" o of
        Just (String s) -> map T.strip $ T.splitOn "," s
        _ -> []
extraiGeneros _ = []

-- Busca gêneros de todos os favoritos
buscaGenerosFavoritos :: [String] -> IO [[T.Text]]
buscaGenerosFavoritos ids = mapM (\idText -> do
    val <- getFilmes idText "i" 
    return $ extraiGeneros val
    ) ids

-- Junta gêneros escolhidos com favoritos e remove duplicados
geraGenerosTotais :: Generos -> IO [T.Text]
geraGenerosTotais g = do
    let escolhidos = map T.pack (generos g)
    generosFav <- buscaGenerosFavoritos (favoritos g)
    return $ nub (escolhidos ++ (concat generosFav))

-- Servidor
main :: IO ()
main = scotty 3000 $ do
    middleware $ cors (const $ Just policy)

    -- Endpoint /hello
    get "/hello" $
        json $ object ["message" .= ("Hello, backend em Haskell!" :: T.Text)]

    -- Endpoint /filmeGeral
    get "/filmeGeral" $ do
        title  <- Web.Scotty.queryParam "title"   -- :: ActionM (Maybe TL.Text)
        option <- Web.Scotty.queryParam "option"  -- :: ActionM (Maybe TL.Text)

        filme <- liftIO $ getFilmes title option
        json filme

    -- Endpoint /generos - Sistema de recomendação por gêneros
    post "/recommend/genero/recentes" $ do
        genero <- jsonData :: ActionM Generos
        generosTotais <- liftIO $ geraGenerosTotais genero
        liftIO $ print ("Gêneros para recomendação:", generosTotais)
        
        -- 1. Buscar filmes aleatórios de várias páginas do TMDB
        filmesRecentes <- liftIO $ getFilmesRecentes "062ee31d45a5eee7e0386f738a396a18"
        
        -- 2. Calcular score e ordenar por compatibilidade
        let filmesComScore = map (\filme -> (filme, pontuarFilme generosTotais filme)) filmesRecentes
            filmesOrdenados = map fst $ sortOn (Down . snd) filmesComScore
            filmesRecomendados = take 20 filmesOrdenados
        
        -- 3. Buscar posters via OMDB
        liftIO $ putStrLn "Buscando posters..."
        filmesComPosters <- liftIO $ mapM adicionarPoster filmesRecomendados
        
        json filmesComPosters