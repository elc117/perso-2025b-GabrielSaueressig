{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text)

main :: IO ()
main = scotty 3000 $ do
  get "/hello" $ do
    json $ object ["message" .= ("Hello, backend em Haskell!" :: Text)]
    