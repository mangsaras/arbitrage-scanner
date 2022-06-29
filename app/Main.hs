module Main where

import System.IO (hFlush, stdout)
import Model.Market (Market (Unknown), idMarket, marketName, apiUrl, description, showAllMarket, parseData)
import Model.FavoriteCoin (FavoriteCoin (Unknown), idCoin, coin, pair, showAllFavoriteCoin, parseDataCoin, addNewFavCoin, saveToFileFavCoin, removeFavCoin)
import Library.Common (MaybeT, liftMaybeT, maybeReadInt, runMaybeT)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

mainMenu = do
    putStrLn "\n\n\n=============== Crypto Arbitrage Scanner  ==============="
    putStrLn "       (a) Show Monitored Market"
    putStrLn "       (b) Setup Favorite Coin"
    putStrLn "       (c) Setup Program"
    putStrLn "       (d) View Scanner"
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
            empty <- prompt "Press enter to go back"
            mainMenu
        "d" -> do
            empty <- prompt "Press enter to go back"
            mainMenu
        "e" -> do
            putStrLn "Thank You !"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            mainMenu

favoriteCoinMenu = do 

    favoriteCoins <- fmap parseDataCoin (readFile "app/Data/FavoriteCoin.txt")
    putStrLn "========================================================="
    putStrLn $ showAllFavoriteCoin favoriteCoins
    putStrLn "(a) Add Favorite Coin  (b) Delete Coin  (c) Main Menu"
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

        "c" -> do
            mainMenu

        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            favoriteCoinMenu

main :: IO ()
main = do
    mainMenu
