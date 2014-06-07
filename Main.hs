module Main where

import qualified Data.Map as Map
import           System.Random

data Node a = Node a | Start | End deriving (Eq, Ord, Show)
type Graph a = Map.Map (Node a) [Node a]

oneOf :: RandomGen g => [a] -> g -> (a,g)
oneOf xs g = ((!!) xs n, g')
  where (n,g') = randomR (0, length xs - 1) g

addHeadAndTail :: a -> a -> [a] -> [a]
addHeadAndTail h t xs = [h] ++ xs ++ [t]

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [_] = []
pairs [x,y] = [(x,y)]
pairs (x:y:xs) = (x,y) : pairs(y:xs)

addPair :: Ord a => Map.Map a [b] -> (a,b) -> Map.Map a [b]
addPair t (x,y) = Map.insert x (y:Map.findWithDefault [] x t) t

addChain :: Ord a => Graph a -> [Node a] -> Graph a
addChain t xs = foldl addPair t (pairs xs)

--

readChainStep :: (Ord a, RandomGen g) => Graph a -> (Node a, g) -> [a]
readChainStep _ (End, _) = []
readChainStep t (Node w, g) =
  case Map.lookup (Node w) t of
       Nothing -> []
       Just xs -> w:readChainStep t (oneOf xs g)

readChain :: (Ord a, RandomGen g) => Graph a -> g -> [a]
readChain t g =
  readChainStep t (case Map.lookup Start t of
                     Nothing -> (End, g)
                     Just ws -> oneOf ws g)

main :: IO ()
main = do
  text <- readFile "corpus.txt"
  let digested = fmap (addHeadAndTail Start End . fmap Node . words) (lines text)
      tree = foldl addChain Map.empty digested
  g <- getStdGen
  print $ unwords $ readChain tree g
