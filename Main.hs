-- OverloadedStrings, OverloadedLists

module Main where

-- HLearn

import qualified Data.Map as Map
import           System.Random
--import qualified Data.Vector as Vector
--import qualified Data.Text as Text

-- Link is a maybe
type Tree a = Map.Map (Maybe a) [(Maybe a)] -- TODO Change [Link a] to (Set (Link a)). It's more correct.

oneOf :: RandomGen g => [a] -> g -> (a,g)
oneOf xs g =
  let (n,g') = randomR (0, length xs - 1) g
  in ((!!) xs n, g')

text :: [String]
text = ["shakespeare was born and brought up in stratford-upon-avon",
        "at the age of 18 he married anne hathaway, with whom he had three children",
        "susanna, and twins hamnet and judith",
        "between 1585 and 1592 he began a successful career in london as an actor writer and part-owner of a playing company called the lord chamberlains men later known as the kings men",
        "he appears to have retired to stratford around 1613 at age 49 where he died three years later",
        "few records of shakespeares private life survive and there has been considerable speculation about such matters as his physical appearance sexuality religious beliefs and whether the works attributed to him were written by others"]


addChain :: Ord a => Tree a -> [a] -> Tree a
addChain t [] = t
addChain t [x] =
  case Map.lookup (Just x) t of
    Nothing -> Map.insert (Just x) [Nothing] t
    Just ys -> Map.insert (Just x) (Nothing:ys) t
addChain t (x:y:xs) =
  case Map.lookup (Just x) t of
    Nothing -> addChain (Map.insert (Just x) [Just y] t) (y:xs)
    Just ys -> addChain (Map.insert (Just x) (Just y:ys) t) (y:xs)

markovStep :: (Ord a, RandomGen g) => g -> Tree a -> Maybe a -> [a]
markovStep _ _ Nothing = []
markovStep g t (Just w) =
  case Map.lookup (Just w) t of
       Nothing -> []
       Just xs -> let (w',g') = oneOf xs g
                  in w:markovStep g' t w'

markov :: (Ord a, RandomGen g) => g -> Tree a -> [a]
markov g t =
  let (w,g') = (case Map.lookup Nothing t of
                    Nothing -> (Nothing, g)
                    Just ws -> oneOf ws g)
  in markovStep g' t w

main :: IO ()
main = do
  g <- getStdGen
  print $ markov g (Map.insert Nothing [Just "he"] graph)
  where digested = map words text
        graph = foldl addChain Map.empty digested
