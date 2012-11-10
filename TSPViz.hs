{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.List
import Control.Monad
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.IO

type Vertex = (Float, Float)
type Edge = (Int, Int)

data Graph = Graph (V.Vector Vertex) [Edge]

data Action
  = AddVertex Vertex
  | AddEdge Edge
  | RemoveEdge Edge

type SharedGraph = TVar Graph

vertexSize :: Float
vertexSize = 5.0

apiID :: String
apiID = "VIZ "

renderGraph :: Graph -> [Picture]
renderGraph (Graph v e) = vertices ++ edges
  where
  vertices = map (\(x,y) -> translate x y $ circleSolid vertexSize) $ V.toList v
  edges = map (\(v1, v2) -> line $ [v V.! v1, v V.! v2]) e

frame :: SharedGraph -> Float -> IO Picture
frame shared seconds = do
  g <- atomically $ readTVar shared
  return $ Pictures $ title : renderGraph g
  where
  title = translate (-300) (300) . scale 0.2 0.2 . Text $ "TSP Visualization, waiting for input on stdin"

launch :: SharedGraph -> IO ()
launch shared = animateIO disp white (frame shared)
  where
  disp = InWindow "TSP tour visualization" (100, 100) (0, 0)

parseAction :: String -> Maybe Action
parseAction (stripPrefix apiID -> Just t) = parse t
parseAction _ = Nothing

parseSep :: Read a => String -> (a, a)
parseSep str = (read a, read $ tail b) 
  where
  (a, b) = span (/= ' ') str

parse :: String -> Maybe Action
parse (stripPrefix "addv " -> Just t) = Just . AddVertex $ parseSep t
parse (stripPrefix "adde " -> Just t) = Just . AddEdge $ parseSep t
parse (stripPrefix "del " -> Just t) = Just . RemoveEdge $ parseSep t
parse _ = Nothing

updateGraph :: Graph -> Action -> Graph
updateGraph (Graph v e) (AddVertex v') = Graph (V.snoc v v') e
updateGraph (Graph v e) (AddEdge e') = Graph v (e':e)
updateGraph (Graph v e) (RemoveEdge e') = Graph v (filter (/= e') e)

parseStream :: SharedGraph -> IO ()
parseStream shared = do
  action <- parseAction <$> getLine
  case action of
    Nothing -> putStrLn "Failed to parse command" >> next
    Just action' -> atomically $ do
      g <- readTVar shared
      let g' = updateGraph g action'
      writeTVar shared g'

  next
  where next = parseStream shared

main :: IO ()
main = do
  shared <- atomically $ newTVar (Graph (V.empty) [])
  forkIO $ parseStream shared
  launch shared
