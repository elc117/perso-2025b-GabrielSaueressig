{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (object, (.=), Value)
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Web.Scotty
import Network.Wai.Middleware.Cors

getFilmes :: String -> T.Text -> IO Value
getFilmes nome option = runReq defaultHttpConfig $ do
    let opts = option =: nome <> "apikey" =: ("ae725e23" :: String)
    r <- req
           GET --metódo
           (http "www.omdbapi.com") --api
           NoReqBody -- sem body 
           jsonResponse -- resposta em json
           opts --opsoens, como a api
    pure (responseBody r :: Value)


main :: IO ()
main = scotty 3000 $ do
  middleware simpleCors
  get "/hello" $ do
    json $ object ["message" .= ("Hello, backend em Haskell!" :: Text)]

  get "/filmeGeral" $ do -- retorna json de filmes
    filme <- liftIO $ getFilmes "Spider Man" (T.pack "t")
    json filme
  