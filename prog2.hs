import Data.List.Split

-- node structure
data Node = Node { key :: String
                 , pathCost :: Float
                 , cameFrom :: String
} deriving (Show, Eq, Ord)

-- function used to initiate all noes. -1 means infinity
initNodes paths acc = if paths == [] then acc
                      else do
                        let first = (head (head paths))
                        let second = (head (tail (head paths)))
                        if not (any ((Node first (- 1) "0")==) acc) && not (any ((Node second (- 1) "0")==) acc) then
                            initNodes (tail paths) acc++[(Node first (- 1) "0")]++[(Node second (- 1) "0")]
                        else
                            if not (any ((Node first (- 1) "0")==) acc) then
                                initNodes (tail paths) acc++[(Node first (- 1) "0")]
                            else
                                initNodes (tail paths) acc++[(Node second (- 1) "0")]


-- main funtion. will be the first one to be called
main = do
    input <- getContents
    let inputListAux = map (splitOn " ") (lines input)
    let inputList = map (filter (not . null)) inputListAux
    let begin = inputList!!((length inputList) - 2)
    let destiny = inputList!!((length inputList) - 1)
    let paths = (init (init inputList))
    let nodes = initNodes paths []
    print nodes
    print paths
    print begin
    print destiny
