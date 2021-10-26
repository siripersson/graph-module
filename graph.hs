
module Graph(Graph,empty,addVertex,addEdge,vertices,neighbors) where

{-  A type that models a graph with finite number of vertices and edges
    The graph is represented by vertices connected via edges.
 -}
 
data Graph a = Void | Graph [a] [(a, a)]
  deriving (Show)  

empty :: Graph a 
empty = Void

{- addVertex graph v
Add a vertex to a graph
RETURNS: the graph with the new vertex added
EXAMPLES: addVertex Void 1 = Graph [1] []
          addVertex (Graph [1,2] [(1,2)]) 3 = (Graph [1,2,3] [(1,2)])
          addVertex (Graph [1,2] []) 2 = (Graph [1,2] [])
 -}
addVertex :: Eq a => Graph a -> a -> Graph a
addVertex Void a = Graph [a] []
addVertex (Graph v e) vertex = if elem vertex v then Graph v e else Graph (vertex:v) e

{- addEdge graph e
Add an edge to a graph
PRE: For an edge, both vertices are in the graph
RETURNS: the graph with the new edge added
EXAMPLES: addEdge (Graph [1,2] []) (1,2) = Graph [1,2] [(1,2)]
          addEdge (Graph [1,2,3] [(1,2)]) (1,3) = (Graph [1,2,3] [(1,2),(1,3)])
 -}
addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge (Graph v e) edge = if elem edge e then Graph v e else Graph v (edge:e)

{- vertices graph
Obtain a list of all vertices in a graph
RETURNS: list of all vertices
EXAMPLES: vertices (Graph [1] []) = [1]
          vertices (Graph [1,3,5,2] [(3,5)]) = [1,3,5,2]
 -}
vertices :: Eq a => Graph a -> [a]
vertices Void = []
vertices (Graph v e) = v 

{- neighbors graph v 
Obtain a list of all neighbors of a given vertex
RETURNS: list of all neighbors
EXAMPLES: neighbors (Graph [1,2] [(1,2)]) 2 = [1]
          neighbors (Graph [1,2,3] [(1,2),(2,3)]) 2 = [1,3]
 -}
neighbors :: Eq a => Graph a -> a -> [a]
neighbors Void v = []
neighbors (Graph v e) vertex = edges e vertex

{- edges e v
Obtain a list of all neighbors from a given vertex
RETURNS: list of v's neighbors
EXAMPLES: edges [(1,2)] 2 = [1]
          edges [(1,2),(2,3)] 2 = [1,3]
 -}
edges :: Eq a => [(a,a)] -> a -> [a]
edges [] vertex = []
edges ((a,b):es) vertex | a==vertex = b : (edges es vertex)
                        | b==vertex = a : (edges es vertex)
                        | otherwise = (edges es vertex)


