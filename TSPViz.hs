{-# LANGUAGE ViewPatterns #-}

-- | Program providing realtime visualization of TSP tours.
--   This module is compiled to a separate binary and then
--   used through an interface exposed over some input stream.
--   Actions are parsed using a separate thread which updates
--   a shared graph structure, which is then rendered.
--
--   Settings of interest are the 'vertexSize' and 'maxBend'
--   properties, which should be scaled based on the input instance.
--
--   The number of tours is currently restricted to 64, based on the
--   internal hashtable used for lookup of overlapping tours.
--
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Bits (clearBit, setBit, complement, popCount, (.&.), shiftR)
import Data.HashTable
import Data.Int (Int32)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Word (Word64)
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry
import Graphics.Gloss.Interface.IO.Animate
import Prelude hiding (lookup)
import System.IO

-- | Vertex type, coordinates in R^2.
type Vertex = (Float, Float)
--
-- | Edge type, identifying two vertices.
type Edge = (Int, Int)

-- | Tour containing a list of edges.
type Tour = [Edge]

-- | Identifier of a tour. Limits internal hashtable to 64 tours.
type TourId = Word64

-- | Graph representation.
data Graph = Graph (V.Vector Vertex) (V.Vector Tour)

-- | Action parsed from input stream.
data Action
  -- | Add a vertex to the graph.
  = AddVertex Vertex
  -- | Add an edge to the graph, belonging to a specific tour.
  | AddEdge TourId Edge
  -- | Remove an edge from the graph, belonging to a specific tour.
  | RemoveEdge TourId Edge
  deriving Show

-- | Type of shared state between render part and input stream parser.
type SharedGraph = TVar (Graph, HashTable Edge Word64)

-- | API identifier used as prefix in input stream, parsed as the first word, with other content being ignored.
apiID :: String
apiID = "VIZ"

-- | Radius of a circle representing a vertex.
vertexSize :: Float
vertexSize = 1.0

-- | Radius of arc being rendered when overalpping edges occur. Lower values results in tightly coupled edges.
maxBend :: Float
maxBend = 0.5

-- | Available colors used in rendering of edges, one color for each tour.
colors :: [Color]
colors = [blue, red, green, yellow, cyan, magenta, rose, violet, azure,
          aquamarine, chartreuse, orange]

-- | Renders a graph using the current graph representation and hashtable indicating overlapping edges.
renderGraph :: Graph -> HashTable Edge Word64 -> IO [Picture]
renderGraph g@(Graph v tours) table = do
  edges <- (join . V.toList) <$> V.mapM (renderTour g table) tourList
  return $ vertices ++ edges
  where
  vertices = map (\(x,y) -> translate x y $ circleSolid vertexSize) $ V.toList v
  tourList =  V.zip tours $ V.enumFromN 0 tourCount
  tourCount = V.length tours

-- | Renders a tour.
renderTour :: Graph -> HashTable Edge Word64 -> (Tour, TourId) -> IO [Picture]
renderTour (Graph v _) table (e, num) = mapM processEdge e
  where
  processEdge (v1, v2) = do
    Just usage <- lookup table (v1, v2)
    let size = popCount usage -- gives number of edges
    let masked = usage .&. shiftR (complement 0) (64 - fromIntegral num)
    let curr = popCount masked -- gives count of current edge
    return . color (edgeColor num) $ drawEdge (v V.! v1) (v V.! v2) size curr

-- | Selects a color based on the tour identifier.
edgeColor :: TourId -> Color
edgeColor num = colors !! (fromIntegral num `mod` length colors)

-- | Draws an edge of a tour.
drawEdge :: Vertex -> Vertex -> Int -> Int -> Picture
drawEdge p1 p2 num curr = if odd num && curr == num - 1 then Line [p1, p2] else mapped
  where
  mapped = positionEdge $ bendEdge num curr

  positionEdge = uncurry translate p1 . rotate angle . translate len 0 . scale len 1
  angle = -(radToDeg $ argV p3)
  p3 = sub p2 p1
  len = magV p3 / 2
  sub (a1, a2) (b1, b2) = (a1 - b1, a2 - b2)

-- | Generates a bent arc used when overlapping edges occur.
bendEdge :: Int -> Int -> Picture
bendEdge num edge = flip . scale 1 yscale $ arc 0 180 1
  where
  flip | odd edge = id
       | otherwise = scale 1 (-1)
  yscale = maxBend - realToFrac edge / (realToFrac num / maxBend)

-- | Frame stepping function used in render.
frame :: SharedGraph -> Float -> IO Picture
frame shared _ = do
  (g, table) <- atomically $ readTVar shared
  Pictures <$> renderGraph g table

-- | Launches the actual visual application thread.
launch :: SharedGraph -> IO ()
launch shared = animateIO disp white (frame shared)
  where
  disp = InWindow "TSP tour visualization" (1000, 1000) (0, 0)

-- | Hashes an edge into representation used by the hash table.
--   Edges are stored for future lookup when detecting overlapping edges between tours.
--   Assumes ordered vertices, with min(v1, v2) being supplied as the first argument.
hashEdge :: Edge -> Int32
hashEdge (v1, v2) = hashInt v1 * hashInt v2

-- | Identifies the initial API ID prefix and consumes the rest of the line.
parseAction :: String -> Maybe Action
parseAction (stripPrefix (apiID ++ " ") -> Just t) = parse $ words t
parseAction _ = Nothing

-- | Parses a tuple of numerical values.
parseVertices :: (Read a, Ord a) => [String] -> (a, a)
parseVertices [a,b] = (min a' b', max a' b')
  where
  a' = read a
  b' = read b

-- | Parses an action from the tokenized input stream.
parse :: [String] -> Maybe Action
parse ("addv" : points) = Just . AddVertex $ parseVertices points
parse ("adde" : tour : t) = Just . AddEdge (read tour) $ parseVertices t
parse ("del" : tour : t) = Just . RemoveEdge (read tour) $ parseVertices t
parse _ = Nothing

-- | Updates the graph representation based on some action, either add vertex, add edge, or remove edge.
updateGraph :: (Graph, HashTable Edge Word64) -> Action -> IO Graph
updateGraph (Graph v e, _) (AddVertex v') = return $ Graph (V.snoc v v') e

updateGraph (Graph v tours, hash) (AddEdge tour e') = do
  prev <- fromMaybe 0 <$> lookup hash e'
  let prev' = setBit prev (fromIntegral tour)
  update hash e' prev'
  return graph
  where graph = Graph v $ updateTour tours tour (e':)

updateGraph (graph@(Graph v tours), hash) (RemoveEdge tour e') = updateHash >> return graph'
  where
  updateHash = do
    prev <- fromMaybe 0 <$> lookup hash e'
    when (prev /= 0) $ do
      let updated = clearBit prev (fromIntegral tour)
      if updated == 0 then delete hash e' else void $ update hash e' updated

  graph' = Graph v $ updateTour tours tour $ filter (/= e')

-- | Updates the tour representation by taking the current vector length into account.
updateTour :: V.Vector Tour -> TourId -> (Tour -> Tour) -> V.Vector Tour
updateTour curr num f | V.length curr >= num' + 1 = V.update curr $
                        V.singleton (num', f $ curr V.! num')
                      | otherwise = V.snoc curr $ f []
  where num' = fromIntegral num

-- | Thread consuming stream input and acting on the specified actions.
--   State shared with render thread is synchronized using a TVar primitive.
parseStream :: SharedGraph -> IO ()
parseStream shared = do
  action <- parseAction <$> getLine
  case action of
    Nothing -> hPutStrLn stderr "Failed to parse command"
    Just action' -> do
      res <- atomically $ readTVar shared
      g' <- updateGraph res action'
      atomically $ writeTVar shared (g', snd res)

  parseStream shared

-- | Main function launching the separate parser and render threads.
main :: IO ()
main = do
  hashTable <- new (==) hashEdge
  shared <- atomically $ newTVar (Graph V.empty V.empty, hashTable)
  _ <- forkIO $ parseStream shared
  launch shared
