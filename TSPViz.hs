{-# LANGUAGE ViewPatterns #-}

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

type Vertex = (Float, Float)
type Edge = (Int, Int)
type Tour = [Edge]
type TourId = Word64

data Graph = Graph (V.Vector Vertex) (V.Vector Tour)

data Action
  = AddVertex Vertex
  | AddEdge TourId Edge
  | RemoveEdge TourId Edge
  deriving Show

type SharedGraph = TVar (Graph, HashTable Edge Word64)

apiID :: String
apiID = "VIZ"

vertexSize, maxBend :: Float
vertexSize = 0.3
maxBend = 0.5

colors :: [Color]
colors = [blue, red, green, yellow, cyan, magenta, rose, violet, azure,
          aquamarine, chartreuse, orange]

renderGraph :: Graph -> HashTable Edge Word64 -> IO [Picture]
renderGraph g@(Graph v tours) table = do
  edges <- (join . V.toList) <$> V.mapM (renderTour g table) tourList
  return $ vertices ++ edges
  where
  vertices = map (\(x,y) -> translate x y $ circleSolid vertexSize) $ V.toList v
  tourList =  V.zip tours $ V.enumFromN 0 tourCount
  tourCount = V.length tours

renderTour :: Graph -> HashTable Edge Word64 -> (Tour, TourId) -> IO [Picture]
renderTour (Graph v _) table (e, num) = mapM processEdge e
  where
  processEdge (v1, v2) = do
    Just usage <- lookup table (v1, v2)
    let size = popCount usage -- gives number of edges
    let masked = usage .&. shiftR (complement 0) (64 - fromIntegral num)
    let curr = popCount masked -- gives count of current edge
    return . color (edgeColor num) $ drawEdge (v V.! v1) (v V.! v2) size curr

edgeColor :: TourId -> Color
edgeColor num = colors !! (fromIntegral num `mod` length colors)

drawEdge :: Vertex -> Vertex -> Int -> Int -> Picture
drawEdge p1 p2 num curr = if odd num && curr == num - 1 then Line [p1, p2] else mapped
  where
  mapped = positionEdge $ bendEdge num curr

  positionEdge = uncurry translate p1 . rotate angle . translate len 0 . scale len 1
  angle = -(radToDeg $ argV p3)
  p3 = sub p2 p1
  len = magV p3 / 2
  sub (a1, a2) (b1, b2) = (a1 - b1, a2 - b2)

bendEdge :: Int -> Int -> Picture
bendEdge num edge = flip . scale 1 yscale $ arc 0 180 1
  where
  flip | odd edge = id
       | otherwise = scale 1 (-1)
  yscale = maxBend - realToFrac edge / (realToFrac num / maxBend)

frame :: SharedGraph -> Float -> IO Picture
frame shared _ = do
  (g, table) <- atomically $ readTVar shared
  Pictures <$> renderGraph g table

launch :: SharedGraph -> IO ()
launch shared = animateIO disp white (frame shared)
  where
  disp = InWindow "TSP tour visualization" (1000, 1000) (0, 0)

-- assumes ordered vertices
hashEdge :: Edge -> Int32
hashEdge (v1, v2) = hashInt v1 * hashInt v2

parseAction :: String -> Maybe Action
parseAction (stripPrefix (apiID ++ " ") -> Just t) = parse $ words t
parseAction _ = Nothing

parseVertices :: (Read a, Ord a) => [String] -> (a, a)
parseVertices [a,b] = (min a' b', max a' b')
  where
  a' = read a
  b' = read b

parse :: [String] -> Maybe Action
parse ("addv" : points) = Just . AddVertex $ parseVertices points
parse ("adde" : tour : t) = Just . AddEdge (read tour) $ parseVertices t
parse ("del" : tour : t) = Just . RemoveEdge (read tour) $ parseVertices t
parse _ = Nothing

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

updateTour :: V.Vector Tour -> TourId -> (Tour -> Tour) -> V.Vector Tour
updateTour curr num f | V.length curr >= num' + 1 = V.update curr $
                        V.singleton (num', f $ curr V.! num')
                      | otherwise = V.snoc curr $ f []
  where num' = fromIntegral num

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

main :: IO ()
main = do
  hashTable <- new (==) hashEdge
  shared <- atomically $ newTVar (Graph V.empty V.empty, hashTable)
  _ <- forkIO $ parseStream shared
  launch shared
