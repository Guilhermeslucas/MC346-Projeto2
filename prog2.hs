-- Nome: Guilherme Lucas da Silva RA:155618
-- Nome: Gustavo Villela Taufic RA: 149211
import Data.List (sortBy)
import Data.Ord (comparing)

-- node structure
data Node = Node String Float String deriving (Show, Eq, Ord)

-- function used to initiate all noes. -1 means infinity
initNodes paths acc =
  if paths == [] then acc
  else do
    let first = (head (head paths))
    let second = (head (tail (head paths)))
    initNodes (tail paths) acc++[(Node first (- 1) "0")]++[(Node second (- 1) "0")]

-- function used to remove the repetitive ocurrences
rmRepeat nodes acc =
  if nodes == [] then acc
  else do
      let first = (head nodes)
      let aux = filter (/=first) nodes
      rmRepeat aux [first]++acc

-- function to init Dijkstra with initial values
initDijkstra [firstNodeKey] nodesList paths [lastNodeKey] = minorPath (updateNode firstNodeKey (Node firstNodeKey 0.0 "0") 0.0 nodesList) paths firstNodeKey lastNodeKey

-- function to calculate the best path
minorPath nodesList paths actualNode endNode =
  if actualNode == endNode then nodesList
  else do
      let (previousNodeKey, nodeKey, transitionCost) = minorCost (filterPaths actualNode paths nodesList) endNode
      let (pNK,nK,tC) = directPath (filterPaths actualNode paths nodesList) actualNode (minorCost (filterPaths nodeKey paths nodesList) endNode)
      if pNK /= "" && nodeKey /= endNode then
        if nK == "" then
          nodesList
        else
          minorPath (updateNode nK (findNode nodesList pNK) tC nodesList) paths nK endNode
      else
        if previousNodeKey == "" then
          nodesList
        else
          minorPath (updateNode nodeKey (findNode nodesList previousNodeKey) transitionCost nodesList) paths nodeKey endNode


-- function to update nodes list
updateNode nodeKey (Node previousNodeKey previousCost previousCameFrom) transitonCost [(Node node cost cameN)] =
  if nodeKey == node then do
              let cost = transitonCost + previousCost
              let cameN = previousNodeKey
              [(Node node cost cameN)]
  else
      [(Node node cost cameN)]

updateNode nodeKey (Node previousNodeKey previousCost previousCameFrom) transitonCost ((Node node cost cameN):r) =
  if nodeKey == node then do
              let cost = transitonCost + previousCost
              let cameN = previousNodeKey
              ((Node node cost cameN):r)
  else
      [(Node node cost cameN)] ++ updateNode nodeKey (Node previousNodeKey previousCost previousCameFrom) transitonCost r

-- search for a node in the nodes list
findNode [node] _ = node

findNode ((Node key cost cameFromNode):r) nodeKey =
  if nodeKey == key then (Node key cost cameFromNode)
  else findNode r nodeKey

-- look for a direct transition to the end node or path with minor cost to a next node
minorCost [] _ = ("", "", -1)

minorCost [(actualNodeKey, nextNodeKey, cost)] endNodeKey = (actualNodeKey, nextNodeKey, cost)

minorCost ((actualNodeKey, nextNodeKey, cost):r) endNodeKey =
  if nextNodeKey == endNodeKey then (actualNodeKey, nextNodeKey, cost)
  else do
      let (otherNodeKey, otherNextNodeKey, otherCost) = minorCost r endNodeKey
      if otherCost == -1 then do (actualNodeKey, nextNodeKey, cost)
      else
          if otherCost > cost then (actualNodeKey, nextNodeKey, cost)
          else (otherNodeKey, otherNextNodeKey, otherCost)


-- function to filter path list, keep just paths from actual node and remove paths to used nodes
filterPaths actualKey [[actualNodeKey, nextNodeKey, cost]] nodesList =
  if actualNodeKey == actualKey then do
      let (Node k c f) = findNode nodesList nextNodeKey
      if c == (-1) then [(actualNodeKey, nextNodeKey, (read cost :: Float))]
      else []
  else []

filterPaths actualKey ([actualNodeKey, nextNodeKey, cost]:r) nodesList =
  if actualNodeKey == actualKey then do
      let (Node k c f) = findNode nodesList nextNodeKey
      if c == (-1) then [(actualNodeKey, nextNodeKey, (read cost :: Float))] ++ (filterPaths actualKey r nodesList)
      else (filterPaths actualKey r nodesList)
  else (filterPaths actualKey r nodesList)

-- search for direct path between two nodes
directPath _ _ (_, "", _) = ("","",-1)

directPath [(actualNodeKey, nextNodeKey, cost)] initNodeKey (_, endNodeKey, _) =
  if actualNodeKey == initNodeKey then do
      if nextNodeKey == endNodeKey then (actualNodeKey, nextNodeKey, cost)
      else ("","",-1)
  else ("","",-1)

directPath ((actualNodeKey, nextNodeKey, cost):r) initNodeKey (_, endNodeKey, _) =
  if actualNodeKey == initNodeKey then do
      if nextNodeKey == endNodeKey then (actualNodeKey, nextNodeKey, cost)
      else directPath r initNodeKey ("",endNodeKey, -1)
  else directPath r initNodeKey ("",endNodeKey, -1)

-- function to return the cost of path
getCost nodes destiny = do
                let (Node key cost _) = (head nodes)
                if key == destiny then cost
                else getCost (tail nodes) destiny


-- functions used to sort the node strucutures
comparingNodes = comparing nodeCost
    where nodeCost (Node _ x _) = x

sortNodes = sortBy comparingNodes

-- function to return just the key of the node
nodeKey (Node key _ _) = key

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
    let [beginAux] = begin
    let [destinyAux] = destiny
    putStrLn ("inicial: " ++ beginAux)
    putStrLn ("final: " ++ destinyAux)
    if (elem (Node destinyAux (-1.0) "0")) nodesUpdated then putStrLn "nada"
    else do
        let cost = getCost nodesUpdated destinyAux
        putStrLn ("custo: " ++ show cost)
        let sorted = sortNodes nodesUpdated
        let filtered = filter (\(Node key cost came) -> cost /= (-1)) sorted
        let maped = map nodeKey filtered
        putStr (unwords maped)
        putStrLn " "
