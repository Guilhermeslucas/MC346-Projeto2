import Data.List.Split

-- node structure
data Node = Node { key :: String
                 , pathCost :: Int
                 , cameFrom :: String
}

-- main funtion. will be the first one to be called
main = do
    input <- getContents
    let inputListAux = map (splitOn " ") (lines input)
    let inputList = map (filter (not . null)) inputListAux
    let begin = inputList!!((length inputList) - 2)
    let destiny = inputList!!((length inputList) - 1)
    let paths = (init (init inputList))
    print paths
    print begin
    print destiny
