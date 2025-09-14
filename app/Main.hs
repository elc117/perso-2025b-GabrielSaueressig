{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (object, (.=), Value)
import Data.Text.Lazy (Text)
import Network.HTTP.Req
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/hello" $ do
    json $ object ["message" .= ("Hello, backend em Haskell!" :: Text)]

  get "/filmes" $ do
    filme <- liftIO $ runReq defaultHttpConfig $ do
        let opts = "t" =: ("Spider man" :: String)
        r <- req
               GET
               (http "www.omdbapi.com") -- domínio
               NoReqBody
               jsonResponse
               (opts <> "apikey" =: ("ae725e23" :: String))
        pure (responseBody r :: Value)
    json filme