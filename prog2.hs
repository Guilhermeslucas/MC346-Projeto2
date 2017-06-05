-- Nome: Guilherme Lucas da Silva RA:155618
-- Nome: Gustavo Villela Taufic RA: 149211
    
-- node structure
data Node = Node String Float String deriving (Show, Eq, Ord)

-- function used to initiate all noes. -1 means infinity
initNodes paths acc = if paths == [] then acc
                      else do
                        let first = (head (head paths))
                        let second = (head (tail (head paths)))
                        initNodes (tail paths) acc++[(Node first (- 1) "0")]++[(Node second (- 1) "0")]

-- function used to remove the repetitive ocurrences
rmRepeat nodes acc = if nodes == [] then acc
                    else do
                        let first = (head nodes)
                        let aux = filter (/=first) nodes
                        rmRepeat aux [first]++acc

-- function to init Dijkstra with initial values
initDijkstra [firstNodeKey] nodesList paths [lastNodeKey]= minorPath (updateNode firstNodeKey (Node firstNodeKey 0.0 "0") 0.0 nodesList) paths firstNodeKey lastNodeKey

-- function to calculate the best path
minorPath nodesList paths actualNode endNode = if actualNode == endNode then nodesList
                                              else do
                                                  let (previousNodeKey, nodeKey, transitionCost) = minorCost (filterPaths actualNode paths nodesList)
                                                  minorPath (updateNode nodeKey (findNode nodesList previousNodeKey) transitionCost nodesList) paths nodeKey endNode


-- function to update nodes list
updateNode nodeKey (Node previousNodeKey previousCost previousCameFrom) transitonCost [(Node node cost cameN)] = if nodeKey == node then do
                                                                                                                              let cost = transitonCost + previousCost
                                                                                                                              let cameN = previousNodeKey
                                                                                                                              [(Node node cost cameN)]
                                                                                                                  else
                                                                                                                      [(Node node cost cameN)]
updateNode nodeKey (Node previousNodeKey previousCost previousCameFrom) transitonCost ((Node node cost cameN):r) = if nodeKey == node then do
                                                                                                                              let cost = transitonCost + previousCost
                                                                                                                              let cameN = previousNodeKey
                                                                                                                              ((Node node cost cameN):r)
                                                                                                                  else
                                                                                                                      [(Node node cost cameN)] ++ updateNode nodeKey (Node previousNodeKey previousCost previousCameFrom) transitonCost r

-- search for a node in the nodes list
findNode [node] _ = node
findNode ((Node key cost cameFromNode):r) nodeKey = if nodeKey == key then (Node key cost cameFromNode)
                                                    else findNode r nodeKey

-- find node transition with minor cost
minorCost [] = ("", "", -1)
minorCost [(actualNodeKey, nextNodeKey, cost)] = (actualNodeKey, nextNodeKey, cost)
minorCost ((actualNodeKey, nextNodeKey, cost):r) = do
                                                let (otherNodeKey, otherNextNodeKey, otherCost) = minorCost r
                                                if otherCost == -1 then (actualNodeKey, nextNodeKey, cost)
                                                else
                                                    if otherCost > cost then (actualNodeKey, nextNodeKey, cost)
                                                    else (otherNodeKey, otherNextNodeKey, otherCost)


-- function to filter path list, keep just paths from actual node and remove paths to used nodes
filterPaths actualKey [[actualNodeKey, nextNodeKey, cost]] nodesList = if actualNodeKey == actualKey then do
                                                                                          let (Node k c f) = findNode nodesList nextNodeKey
                                                                                          if c == (-1) then [(actualNodeKey, nextNodeKey, (read cost :: Float))]
                                                                                          else []
                                                                       else []
filterPaths actualKey ([actualNodeKey, nextNodeKey, cost]:r) nodesList = if actualNodeKey == actualKey then do
                                                                                          let (Node k c f) = findNode nodesList nextNodeKey
                                                                                          if c == (-1) then [(actualNodeKey, nextNodeKey, (read cost :: Float))] ++ (filterPaths actualKey r nodesList)
                                                                                          else (filterPaths actualKey r nodesList)
                                                                         else (filterPaths actualKey r nodesList)

-- main funtion. will be the first one to be called
main = do
    input <- getContents
    let inputListAux = map words (lines input)
    let inputList = map (filter (not . null)) inputListAux
    let begin = inputList!!((length inputList) - 2)
    let destiny = inputList!!((length inputList) - 1)
    let paths = (init (init inputList))
    let nodesAux = initNodes paths []
    let nodes = rmRepeat nodesAux []
    let nodesUpdated = initDijkstra begin nodes paths destiny
    print nodes
    print paths
    print begin
    print destiny
    print nodesUpdated
