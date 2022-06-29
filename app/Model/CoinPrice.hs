module Model.CoinPrice where

import Data.List

data CoinPrice
    = CoinPrice
        { coinPair :: String
        , price :: String
        }
    | Unknown
    deriving (Show, Eq)

parseCoinPrice :: String -> [CoinPrice]
parseCoinPrice rawContent = map parseSingleCoinPrice (lines rawContent)

parseSingleCoinPrice :: String -> CoinPrice
parseSingleCoinPrice str = case words str of
    (a : b) -> initCoinPrice a b
    _ -> Unknown

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
        extractData Nothing = Unknown

        getData :: [CoinPrice] -> CoinPrice -> String
        getData [] chosenItem  = "0"
        getData (coinPrice : coinPrices) chosenItem
            | coinPrice == chosenItem = (price coinPrice)
            | otherwise = getData coinPrices chosenItem

    let tempPrice =
            if (extractData dataExist) == Unknown
                then "0"
                else getData listData (extractData dataExist)

    return tempPrice