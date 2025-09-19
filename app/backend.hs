{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser, Object)
import qualified Data.Text as T
import Network.HTTP.Req
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Aeson.KeyMap as KM

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

-- Política de CORS
policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy
           { corsRequestHeaders = ["Content-Type"]
           , corsMethods = ["GET","POST","OPTIONS"]
           }

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

    -- Endpoint /generos
    post "/recommend/genero" $ do
        genero <- jsonData :: ActionM Generos
        liftIO $ print genero
        generosTotais <- liftIO $ geraGenerosTotais genero
        liftIO $ print generosTotais
        json $ object ["status" .= ("ok" :: String), "generos_totais" .= generosTotais]

