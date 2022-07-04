{-# LANGUAGE OverloadedStrings #-}

module Model.CoinPrice where

import Data.List ( find )
import Text.Printf ( printf )
import Model.FavoriteCoin (FavoriteCoin (Unknown), idCoin, coin, pair, parseDataCoin)
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Char
import qualified Data.Text as T     

data CoinPrice
    = CoinPrice
        { coinPair :: String
        , price :: String
        }
    | UnknownCoin
    deriving (Show, Eq)

parseCoinPrice :: String -> [CoinPrice]
parseCoinPrice rawContent = map parseSingleCoinPrice (lines rawContent)

parseSingleCoinPrice :: String -> CoinPrice
parseSingleCoinPrice str = case words str of
    (a : b) -> initCoinPrice a b
    _ -> UnknownCoin

initCoinPrice :: String -> [String] -> CoinPrice
initCoinPrice coinPair price =
    CoinPrice
        { coinPair = coinPair
        , price = unwords price
        }


showAllCoinPrice :: [CoinPrice] -> String
showAllCoinPrice [] = replicate 58 '='
showAllCoinPrice (coinPrice : coinPrices) =
    "Coin/Pair : " ++ show (coinPair coinPrice)
        ++ "\nPrice : "
        ++ price coinPrice
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAllCoinPrice coinPrices

getPrice :: [CoinPrice] -> String -> IO String
getPrice listData choice = do
    let dataExist = find (\item -> coinPair item == choice) listData

        extractData :: Maybe CoinPrice -> CoinPrice
        extractData (Just a) = a
        extractData Nothing = UnknownCoin

        getData :: [CoinPrice] -> CoinPrice -> String
        getData [] chosenItem  = "0"
        getData (coinPrice : coinPrices) chosenItem
            | coinPrice == chosenItem = price coinPrice
            | otherwise = getData coinPrices chosenItem

    let tempPrice =
            if extractData dataExist == UnknownCoin
                then "0"
                else getData listData (extractData dataExist)

    return tempPrice

scannerPriceOffline :: String -> String -> IO String
scannerPriceOffline coin pair = do
    
    binance <- fmap parseCoinPrice (readFile "app/Data/Binance.txt")
    indodax <- fmap parseCoinPrice (readFile "app/Data/Indodax.txt")
    kucoin <- fmap parseCoinPrice (readFile "app/Data/Kucoin.txt")

    let coinPair = coin ++ "/" ++ pair
    priceBinance <- getPrice binance coinPair
    priceIndodax <- getPrice indodax coinPair
    priceKucoin <- getPrice kucoin coinPair

    let binanceDouble = read priceBinance :: Double
    let indodaxDouble = read priceIndodax :: Double
    let kucoinDouble = read priceKucoin :: Double

    let diffBinanceIndodax = ((indodaxDouble-binanceDouble)/binanceDouble)*100
    let diffBinanceKucoin = ((kucoinDouble-binanceDouble)/binanceDouble)*100
    let diffIndodaxBinance = ((binanceDouble-indodaxDouble)/indodaxDouble)*100
    let diffIndodaxKucoin = ((kucoinDouble-indodaxDouble)/indodaxDouble)*100
    let diffKucoinBinance = ((binanceDouble-kucoinDouble)/kucoinDouble)*100
    let diffKucoinIndodax = ((indodaxDouble-kucoinDouble)/kucoinDouble)*100

    let result = "\n========================================================="
                ++ "\nCoin/Pair : " ++ coinPair
                ++ ("\nBinance Price : " ++ priceBinance)
                ++ ("\nIndodax Price : " ++ priceIndodax)
                ++ ("\nKucoin Price : " ++ priceKucoin)
                ++ ("\n" ++ replicate 29 '-')
                ++ printf "\nBinance -> Indodax : %.2f%%" diffBinanceIndodax
                ++ printf "\nBinance -> Kucoin : %.2f%%" diffBinanceKucoin
                ++ printf "\nIndodax -> Binance : %.2f%%" diffIndodaxBinance
                ++ printf "\nIndodax -> Kucoin : %.2f%%" diffIndodaxKucoin
                ++ printf "\nKucoin -> Binance : %.2f%%" diffKucoinBinance
                ++ printf "\nKucoin -> Indodax : %.2f%%" diffKucoinIndodax
                ++ "\n========================================================="

    return result

scannerPrice :: String -> String -> IO String
scannerPrice coin pair = do
    
    let coinPair = coin ++ "/" ++ pair

    priceBinance <- getBinancePrice coin pair
    priceIndodax <- getIndodaxPrice coin pair
    priceKucoin <- getKucoinPrice coin pair

    let binanceDouble = read priceBinance :: Double
    let indodaxDouble = read priceIndodax :: Double
    let kucoinDouble = read priceKucoin :: Double

    let diffBinanceIndodax = ((indodaxDouble-binanceDouble)/binanceDouble)*100
    let diffBinanceKucoin = ((kucoinDouble-binanceDouble)/binanceDouble)*100
    let diffIndodaxBinance = ((binanceDouble-indodaxDouble)/indodaxDouble)*100
    let diffIndodaxKucoin = ((kucoinDouble-indodaxDouble)/indodaxDouble)*100
    let diffKucoinBinance = ((binanceDouble-kucoinDouble)/kucoinDouble)*100
    let diffKucoinIndodax = ((indodaxDouble-kucoinDouble)/kucoinDouble)*100

    rBuy <- checkRecomendedBuy binanceDouble indodaxDouble kucoinDouble
    rSell <- checkRecomendedSell binanceDouble indodaxDouble kucoinDouble
    

    let result = "\n========================================================="
                ++ "\nCoin/Pair : " ++ coinPair
                ++ ("\nBinance Price : " ++ priceBinance)
                ++ ("\nIndodax Price : " ++ priceIndodax)
                ++ ("\nKucoin Price : " ++ priceKucoin)
                ++ ("\n" ++ replicate 29 '-')
                ++ printf "\nBinance -> Indodax : %.2f%%" diffBinanceIndodax
                ++ printf "\nBinance -> Kucoin : %.2f%%" diffBinanceKucoin
                ++ printf "\nIndodax -> Binance : %.2f%%" diffIndodaxBinance
                ++ printf "\nIndodax -> Kucoin : %.2f%%" diffIndodaxKucoin
                ++ printf "\nKucoin -> Binance : %.2f%%" diffKucoinBinance
                ++ printf "\nKucoin -> Indodax : %.2f%%" diffKucoinIndodax
                ++ "\n" ++ rBuy
                ++ "\n" ++ rSell
                ++ "\n========================================================="

    return result



scannerPriceAllFavCoin :: [FavoriteCoin] -> IO ()
scannerPriceAllFavCoin [] = return()
scannerPriceAllFavCoin (favoriteCoin:favoriteCoins) =
    do
        temp <- scannerPrice (coin favoriteCoin) (pair favoriteCoin)
        putStrLn temp
        scannerPriceAllFavCoin favoriteCoins

getBinancePrice :: String -> String -> IO String
getBinancePrice coin pair = 
    do 
        r <- get ("https://api.binance.me/api/v3/ticker/price?symbol=" ++ coin ++ pair)
        let price =  T.unpack(r ^. responseBody . key "price" . _String)
        return price

getIndodaxPrice :: String -> String -> IO String
getIndodaxPrice coin pair = 
    do 
        r <- get ("https://indodax.com/api/" ++ lowerCase coin ++ "_" ++ lowerCase pair ++ "/ticker")
        let price = T.unpack(r ^. responseBody . key "ticker".  key "last". _String)
        return price

getKucoinPrice :: String -> String -> IO String
getKucoinPrice coin pair = 
    do 
        r <- get ("https://api.kucoin.com/api/v1/market/orderbook/level1?symbol=" ++ coin ++ "-" ++ pair)
        let price = T.unpack(r ^. responseBody . key "data" . key "price" . _String)
        return price

lowerCase :: String -> String
lowerCase [] = []
lowerCase (x:xs) = if x `elem` ['A'..'Z'] 
               then chr (ord x + 32) : lowerCase xs 
               else x : lowerCase xs  

checkRecomendedBuy :: Double -> Double -> Double -> IO String
checkRecomendedBuy binancePrice indodaxPrice kucoinPrice = do
    let recomendedBuy
            | binancePrice < indodaxPrice && binancePrice < kucoinPrice
                = "Recomended Buy in Binance Market with price : " ++ show binancePrice
            | indodaxPrice < binancePrice && indodaxPrice < kucoinPrice
                = "Recomended Buy in Indodax Market with price : " ++ show indodaxPrice
            | kucoinPrice < binancePrice && kucoinPrice < indodaxPrice
                = "Recomended Buy in Kucoin Market with price : " ++ show kucoinPrice
            | otherwise = ""
    return recomendedBuy

checkRecomendedSell :: Double -> Double -> Double -> IO String
checkRecomendedSell binancePrice indodaxPrice kucoinPrice = do
    let recomendedSell
            | binancePrice > indodaxPrice && binancePrice > kucoinPrice
                = "Recomended Sell in Binance Market with price : " ++ show binancePrice
            | indodaxPrice > binancePrice && indodaxPrice > kucoinPrice
                = "Recomended Sell in Indodax Market with price : " ++ show indodaxPrice
            | kucoinPrice > binancePrice && kucoinPrice > indodaxPrice
                = "Recomended Sell in Kucoin Market with price : " ++ show kucoinPrice
            | otherwise = ""
    return recomendedSell