module Model.FavoriteCoin where

import Data.List

data FavoriteCoin
    = FavoriteCoin
        { idCoin :: Int
        , coin :: String
        , pair :: String
        }
    | Unknown
    deriving (Show, Eq)

parseDataCoin :: String -> [FavoriteCoin]
parseDataCoin rawContent = map parseSingleDataCoin (lines rawContent)

parseSingleDataCoin :: String -> FavoriteCoin
parseSingleDataCoin str = case words str of
    (a : b : c) -> initFavoriteCoin a b c
    _ -> Unknown

initFavoriteCoin :: String -> String -> [String] -> FavoriteCoin
initFavoriteCoin idCoin coin pair =
    FavoriteCoin
        { idCoin = read idCoin
        , coin = coin
        , pair = unwords pair
        }

showAllFavoriteCoin :: [FavoriteCoin] -> String
showAllFavoriteCoin [] = replicate 58 '='
showAllFavoriteCoin (favoriteCoin : favoriteCoins) =
    "No : " ++ show (idCoin favoriteCoin)
        ++ "\nCoin Symbol : "
        ++ coin favoriteCoin
        ++ "\nPair : "
        ++ pair favoriteCoin
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAllFavoriteCoin favoriteCoins

addNewFavCoin :: [FavoriteCoin] -> String -> String -> IO [FavoriteCoin]
addNewFavCoin oldListData coin pair = do
    let lastId =
            if null oldListData
                then 0
                else idCoin $ last oldListData
        newId = lastId + 1
        newFavCoin =
            FavoriteCoin
                { idCoin = newId
                , coin = coin
                , pair = pair
                }
    let newListData = oldListData ++ [newFavCoin]
    return newListData

removeFavCoin :: [FavoriteCoin] -> Int -> IO [FavoriteCoin]
removeFavCoin listData choice = do
    let dataExist = find (\item -> (idCoin item) == choice) listData

        extractData :: Maybe FavoriteCoin -> FavoriteCoin
        extractData (Just a) = a
        extractData Nothing = Unknown

        removeData :: [FavoriteCoin] -> FavoriteCoin -> [FavoriteCoin]
        removeData [] chosenItem = []
        removeData (favoriteCoin : favoriteCoins) chosenItem
            | favoriteCoin == chosenItem = removeData favoriteCoins chosenItem
            | otherwise = [favoriteCoin] ++ removeData favoriteCoins chosenItem

        updateIdData :: [FavoriteCoin] -> Int -> [FavoriteCoin]
        updateIdData [] newId = []
        updateIdData (favoriteCoin : favoriteCoins) newId
            | newId == 0 = [favoriteCoin{idCoin = 1}] ++ updateIdData favoriteCoins 1
            | otherwise = [favoriteCoin{idCoin = newId + 1}] ++ updateIdData favoriteCoins (newId + 1)

    let newList =
            if (extractData dataExist) == Unknown
                then listData
                else updateIdData (removeData listData (extractData dataExist)) 0

    if (extractData dataExist) == Unknown
        then putStrLn "Data not found. Please check your Coin ID"
        else putStrLn "Successfully Remove Coin !"

    return newList

saveToFileFavCoin :: [FavoriteCoin] -> IO ()
saveToFileFavCoin listData = do
    let convertToString :: [FavoriteCoin] -> String
        convertToString [] = ""
        convertToString (favoriteCoin : favoriteCoins) =
            show (idCoin favoriteCoin)
                ++ " "
                ++ coin favoriteCoin
                ++ " "
                ++ pair favoriteCoin
                ++ "\n"
                ++ convertToString favoriteCoins
    let saveToFileFavCoin = init $ convertToString listData
    writeFile "app/Data/FavoriteCoin.txt" saveToFileFavCoin