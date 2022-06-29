module Main where

import System.IO (hFlush, stdout)
import Model.Market (Market (Unknown), idMarket, marketName, apiUrl, description, showAllMarket, parseData)
import Model.FavoriteCoin (FavoriteCoin (Unknown), idCoin, coin, pair, showAllFavoriteCoin, parseDataCoin, addNewFavCoin, saveToFileFavCoin, updateFavCoin, removeFavCoin)
import Model.CoinPrice (CoinPrice (UnknownCoin), coinPair, price, parseCoinPrice, showAllCoinPrice, getPrice, scannerPrice, scannerPriceAllFavCoin)
import Library.Common (MaybeT, liftMaybeT, maybeReadInt, runMaybeT)
import Text.Printf

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

mainMenu = do
    putStrLn "\n\n\n=============== Crypto Arbitrage Scanner  ==============="
    putStrLn "       (a) Show Monitored Market"
    putStrLn "       (b) Setup Favorite Coin"
    putStrLn "       (c) View Scanner"
    putStrLn "       (e) Exit Program"
    putStrLn "========================================================="
    choice <- prompt "Input choice: "
    case choice of 
        "a" -> do
            markets <- fmap parseData (readFile "app/Data/Market.txt")
            putStrLn "========================================================="
            putStrLn $ showAllMarket markets
            empty <- prompt "Press enter to go back"
            mainMenu
        "b" -> do
            favoriteCoinMenu
        "c" -> do
            scannerMenu
        "e" -> do
            putStrLn "Thank You !"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            mainMenu

favoriteCoinMenu = do 

    favoriteCoins <- fmap parseDataCoin (readFile "app/Data/FavoriteCoin.txt")
    putStrLn "========================================================="
    putStrLn $ showAllFavoriteCoin favoriteCoins
    putStrLn "(a) Add Favorite Coin (b) Update Favorite Coin  (c) Delete Coin  (d) Main Menu"
    choiceFavCoin <- prompt "Input choice: "

    case choiceFavCoin of
        "a" -> do
            putStrLn "You're about to add some coin .. "
            coin <- prompt "Coin Symbol : "
            pair <- prompt "Pair : "
            newFavCoin <- addNewFavCoin favoriteCoins coin pair
            saveToFileFavCoin newFavCoin
            emptyPrompt <- prompt "Successfully added new favorite coin ! Press enter to continue."
            favoriteCoinMenu
        
        "b" -> do
            putStrLn "You're about to update some favorite coin ! "
            putStr "Insert Coin ID : "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            coin <- prompt "Coin Symbol : "
            pair <- prompt "Pair : "
            newListFavCoin <- updateFavCoin favoriteCoins choice coin pair
            saveToFileFavCoin newListFavCoin
            emptyPrompt <- prompt "Press enter to continue."
            favoriteCoinMenu

        "c" -> do
            putStrLn "You're about to delete some favorite coin ! "
            putStr "Insert Coin ID : "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            newListFavCoin <- removeFavCoin favoriteCoins choice
            saveToFileFavCoin newListFavCoin
            emptyPrompt <- prompt "Press enter to continue."
            favoriteCoinMenu

        "d" -> do
            mainMenu

        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            favoriteCoinMenu

scannerMenu = do
    putStrLn "\n\n\n=============== Crypto Arbitrage Scanner  ==============="
    putStrLn "       (a) Input Coin"
    putStrLn "       (b) Scan Based on Favorite Coin"
    putStrLn "       (d) Main Menu"
    putStrLn "========================================================="
    choiceScanMenu <- prompt "Input choice: "
    case choiceScanMenu of 
        "a" -> do
            coinPair <- prompt "Coin/Pair : "
            tempResult <- scannerPrice coinPair
            putStrLn tempResult
            empty <- prompt "Press enter to go back"
            scannerMenu
        "b" -> do
            favoriteCoins <- fmap parseDataCoin (readFile "app/Data/FavoriteCoin.txt")
            putStrLn $ scannerPriceAllFavCoin favoriteCoins
            empty <- prompt "Press enter to go back"
            scannerMenu
        "d" -> do
            mainMenu
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            mainMenu



main :: IO ()
main = do
    mainMenu
