{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.List
import Control.Monad
import qualified Data.Vector as V
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.IO

-- TODO List:
-- * Smart rendering of multiple edges sharing the same tuple of vertices, look into displacement.

type Vertex = (Float, Float)
type Edge = (Int, Int)
type Tour = [Edge]
type TourId = Int

data Graph = Graph (V.Vector Vertex) (V.Vector Tour)

data Action
  = AddVertex Vertex
  | AddEdge TourId Edge
  | RemoveEdge TourId Edge
  deriving Show

type SharedGraph = TVar Graph

vertexSize :: Float
vertexSize = 2.0

apiID :: String
apiID = "VIZ"

colors :: [Color]
colors = [blue, red, green, yellow, cyan, magenta, rose, violet, azure,
          aquamarine, chartreuse, orange]

renderGraph :: Graph -> [Picture]
renderGraph g@(Graph v tours) = vertices ++ edges
  where
  vertices = map (\(x,y) -> translate x y $ circleSolid vertexSize) $ V.toList v
  edges = join $ V.toList $ V.map (renderTour g) $ V.zip tours $ V.enumFromN 0 tourCount
  tourCount = V.length tours

renderTour :: Graph -> (Tour, TourId) -> [Picture]
renderTour (Graph v _) (e, num) = map (\(v1, v2) -> color chosenColor $ line [v V.! v1, v V.! v2]) e
  where chosenColor = colors !! (num `mod` length colors)

frame :: SharedGraph -> Float -> IO Picture
frame shared _ = do
  g <- atomically $ readTVar shared
  return $ Pictures $ renderGraph g

launch :: SharedGraph -> IO ()
launch shared = animateIO disp white (frame shared)
  where
  disp = InWindow "TSP tour visualization" (500, 500) (0, 0)

parseAction :: String -> Maybe Action
parseAction (stripPrefix (apiID ++ " ") -> Just t) = parse $ words t
parseAction _ = Nothing

parseTuple :: Read a => [String] -> (a, a)
parseTuple [a,b] = (read a, read b) 

parse :: [String] -> Maybe Action
parse ("addv" : points) = Just . AddVertex $ parseTuple points
parse ("adde" : tour : t) = Just . AddEdge (read tour) $ parseTuple t
parse ("del" : tour : t) = Just . RemoveEdge (read tour) $ parseTuple t
parse _ = Nothing

updateGraph :: Graph -> Action -> Graph
updateGraph (Graph v e) (AddVertex v') = Graph (V.snoc v v') e
updateGraph (Graph v tours) (AddEdge tour e') = Graph v $
  updateTour tours tour (e':)
updateGraph (Graph v tours) (RemoveEdge tour e') = Graph v $
  updateTour tours tour $ filter (/= e')

updateTour :: V.Vector Tour -> TourId -> (Tour -> Tour) -> V.Vector Tour
updateTour curr num f | V.length curr >= num + 1 = V.update curr $
                        V.singleton (num, f $ curr V.! num)
                      | otherwise = V.snoc curr $ f []

parseStream :: SharedGraph -> IO ()
parseStream shared = do
  action <- parseAction <$> getLine
  case action of
    Nothing -> hPutStrLn stderr "Failed to parse command"
    Just action' -> atomically $ do
      g <- readTVar shared
      let g' = updateGraph g action'
      writeTVar shared g'

  parseStream shared

main :: IO ()
main = do
  shared <- atomically . newTVar $ Graph V.empty V.empty
  _ <- forkIO $ parseStream shared
  launch shared
