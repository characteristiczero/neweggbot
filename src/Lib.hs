{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Lib
    ( startApp
    ) where

import    GHC.Generics (Generic)

import    Network.HTTP.Client      (newManager,Manager)
import    Network.HTTP.Client.TLS  (tlsManagerSettings)

import    Web.Telegram.API.Bot 
import    Web.Telegram.API.Bot (ChatId, sendMessage, sendMessageRequest)
import    Web.Telegram.API.Bot (ChatId( ChatChannel ) )

import    Data.Text (Text)
import    Data.Maybe

import    qualified Data.Text as T 
import    qualified Data.ByteString.Lazy as B

import    Control.Lens
import    Network.Wreq
import    Data.Aeson (decode)
import    Data.Aeson.Lens
import    Data.Aeson.Types (Parser, Value, FromJSON, parseJSON, parseMaybe) 
import    Control.Concurrent (threadDelay)

products :: [(Text,Bool,Bool)]
products = [
            ("9SIAD9Y6TE1919",False,False),
            ("N82E16814202293",False,False) 
           ]

owsurl :: Text
owsurl = "http://www.ows.newegg.com/Products.egg/"

produrl :: Text 
produrl = "https://www.newegg.com/Product/Product.aspx?Item="

genurl :: Text -> Text
genurl prodid = T.concat [produrl, prodid]

parseB :: Value -> Parser Bool
parseB x = parseJSON x

parseT :: [Value] -> Bool
parseT [] = False
parseT v = Data.Maybe.fromMaybe False $ parseMaybe parseB $ head v

updateChannel :: Token -> ChatId -> Manager -> [(Text,Bool,Bool)] -> IO ()
updateChannel t c m prods = do
    p <- mapM (updateStock t c m) prods 
    putStrLn "check done"
    updateChannel t c m p 

updateStock :: Token ->  ChatId -> Manager -> (Text,Bool,Bool) -> IO ((Text,Bool,Bool))
updateStock t c m (prodid, updated, instock) = do
    threadDelay 10000
    r <- get $ T.unpack $ T.concat [owsurl,prodid]
    let x = r ^.. responseBody . key "Basic". key "Instock"
        nStock = parseT x 
        nUpdated = if nStock == instock then False else True
        sendQ = ((nUpdated /= updated) && nStock)
    case sendQ of 
        True -> do 
            let message = genurl prodid 
                request = sendMessageRequest c message
            sendMessage t request m 
            --updateStock t c m (prodid, nUpdated, nStock)
            putStrLn $ show $ T.concat["update sent :" , prodid]
            return (prodid,nUpdated,nStock)
        False -> do 
            --updateStock t c m (prodid, nUpdated, nStock)
            putStrLn $ show $ T.concat["no update : " , prodid]
            return (prodid,nUpdated,nStock)

startApp :: IO ()
startApp = do
    putStrLn "bot started"
    let token = Token "token" 
    let chatId = ChatChannel "channel" 
    manager <- newManager tlsManagerSettings
    updateChannel token chatId manager products  


