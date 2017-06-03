import Data.List.Split

-- main funtion. will be the first one to be called
main = do
    input <- getContents
    let inputListAux = map (splitOn " ") (lines input)
    let inputList = map (filter (not . null)) inputListAux
    print inputList
