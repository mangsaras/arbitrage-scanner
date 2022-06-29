module Model.Market where

data Market
    = Market
        { idMarket :: Int
        , marketName :: String
        , apiUrl :: String
        , description :: String
        }
    | Unknown
    deriving (Show, Eq)

parseData :: String -> [Market]
parseData rawContent = map parseSingleData (lines rawContent)

parseSingleData :: String -> Market
parseSingleData str = case words str of
    (a : b : c : d) -> initMarket a b c d
    _ -> Unknown

initMarket :: String -> String -> String -> [String] -> Market
initMarket idMarket marketName apiUrl description =
    Market
        { idMarket = read idMarket
        , marketName = marketName
        , apiUrl = apiUrl
        , description = unwords description
        }

showAllMarket :: [Market] -> String
showAllMarket [] = replicate 58 '='
showAllMarket (market : markets) =
    "No : " ++ show (idMarket market)
        ++ "\nName : "
        ++ marketName market
        ++ "\nApi Url : "
        ++ apiUrl market
        ++ "\nDescription : "
        ++ description market
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAllMarket markets