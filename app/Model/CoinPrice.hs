module Model.CoinPrice where

import Data.List
import Text.Printf
import Model.FavoriteCoin (FavoriteCoin (Unknown), idCoin, coin, pair, parseDataCoin)

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
    let dataExist = find (\item -> (coinPair item) == choice) listData

        extractData :: Maybe CoinPrice -> CoinPrice
        extractData (Just a) = a
        extractData Nothing = UnknownCoin

        getData :: [CoinPrice] -> CoinPrice -> String
        getData [] chosenItem  = "0"
        getData (coinPrice : coinPrices) chosenItem
            | coinPrice == chosenItem = (price coinPrice)
            | otherwise = getData coinPrices chosenItem

    let tempPrice =
            if (extractData dataExist) == UnknownCoin
                then "0"
                else getData listData (extractData dataExist)

    return tempPrice

scannerPrice :: String -> IO String
scannerPrice coinPair = do
    binance <- fmap parseCoinPrice (readFile "app/Data/Binance.txt")
    ftx <- fmap parseCoinPrice (readFile "app/Data/FTX.txt")
    kucoin <- fmap parseCoinPrice (readFile "app/Data/Kucoin.txt")
    
    priceBinance <- getPrice binance coinPair
    priceFTX <- getPrice ftx coinPair
    priceKucoin <- getPrice kucoin coinPair

    let binanceDouble = read priceBinance :: Double 
    let ftxDouble = read priceFTX :: Double 
    let kucoinDouble = read priceKucoin :: Double 

    let diffBinanceFtx = ((ftxDouble-binanceDouble)/binanceDouble)*100
    let diffBinanceKucoin = ((kucoinDouble-binanceDouble)/binanceDouble)*100
    let diffFtxBinance = ((binanceDouble-ftxDouble)/ftxDouble)*100
    let diffFtxKucoin = ((kucoinDouble-ftxDouble)/ftxDouble)*100
    let diffKucoinBinance = ((binanceDouble-kucoinDouble)/kucoinDouble)*100
    let diffKucoinFTX = ((ftxDouble-kucoinDouble)/kucoinDouble)*100

    let result = "=========================================================" 
                ++ "\nCoin/Pair : " ++ coinPair
                ++ ("\nBinance Price : " ++ priceBinance)
                ++ ("\nFTX Price : " ++ priceFTX)
                ++ ("\nKucoin Price : " ++ priceKucoin)
                ++ ("\n" ++ replicate 29 '-')
                ++ printf "\nBinance -> FTX : %.2f%%" diffBinanceFtx
                ++ printf "\nBinance -> Kucoin : %.2f%%" diffBinanceKucoin
                ++ printf "\nFTX -> Binance : %.2f%%" diffFtxBinance
                ++ printf "\nFTX -> Kucoin : %.2f%%" diffFtxKucoin
                ++ printf "\nKucoin -> Binance : %.2f%%" diffKucoinBinance
                ++ printf "\nKucoin -> FTX : %.2f%%" diffKucoinFTX
                ++ "\n========================================================="

    return result


scannerPriceAllFavCoin [] = replicate 58 '='
scannerPriceAllFavCoin (favoriteCoin : favoriteCoins) = do
        (coin favoriteCoin ++ "/" ++ pair favoriteCoin)
        ++ "\n"
        ++ scannerPriceAllFavCoin favoriteCoins