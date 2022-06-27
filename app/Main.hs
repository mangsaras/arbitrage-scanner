module Main where

import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

runProgram messages = do
    putStrLn "\n\n\n=============== Crypto Arbitrage Scanner  ==============="
    putStrLn "       (a) Show all exchange"
    putStrLn "       (b) Setup favorite coin"
    putStrLn "       (c) Setup program"
    putStrLn "       (d) View Scanner"
    putStrLn "       (e) Exit program"
    putStrLn "========================================================="
    choice <- prompt "Input choice: "
    case choice of
        "a" -> do
            empty <- prompt "Press enter to go back"
            runProgram messages
        "b" -> do
            empty <- prompt "Press enter to go back"
            runProgram messages
        "c" -> do
            empty <- prompt "Press enter to go back"
            runProgram messages
        "d" -> do
            empty <- prompt "Press enter to go back"
            runProgram messages
        "e" -> do
            putStrLn "Exiting program..."
            putStrLn "Goodbye!"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram messages


main :: IO ()
main = do
    runProgram []
