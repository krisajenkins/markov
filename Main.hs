-- OverloadedStrings, OverloadedLists

module Main where

-- HLearn
--import qualified Data.Vector as Vector
--import qualified Data.Text as Text
import qualified Data.Map as Map
import           System.Random

type Tree a = Map.Map (Maybe a) [Maybe a]

oneOf :: RandomGen g => [a] -> g -> (a,g)
oneOf xs g =
  let (n,g') = randomR (0, length xs - 1) g
  in ((!!) xs n, g')

addHeadAndTail :: a -> a -> [a] -> [a]
addHeadAndTail h t xs = [h] ++ xs ++ [t]

addChain :: Ord a => Tree a -> [Maybe a] -> Tree a
addChain t [] = t
addChain t [x,y] = Map.insert x (y:ys) t
  where ys = Map.findWithDefault [] x t
addChain t (x:y:xs) = addChain (addChain t [x,y]) (y:xs)

readChainStep :: (Ord a, RandomGen g) => Tree a -> (Maybe a, g) -> [a]
readChainStep _ (Nothing, g) = []
readChainStep t (Just w, g) =
  case Map.lookup (Just w) t of
       Nothing -> []
       Just xs -> w:readChainStep t (oneOf xs g)

readChain :: (Ord a, RandomGen g) => Tree a -> g -> [a]
readChain t g =
  readChainStep t (case Map.lookup Nothing t of
                     Nothing -> (Nothing, g)
                     Just ws -> oneOf ws g)

text :: [String]
text = ["shakespeare was born and brought up in stratford-upon-avon",
        "at the age of 18 he married anne hathaway with whom he had three children",
        "susanna and twins hamnet and judith",
        "between 1585 and 1592 he began a successful career in london as an actor writer and part-owner of a playing company called the lord chamberlains men later known as the kings men",
        "he appears to have retired to stratford around 1613 at age 49 where he died three years later",
        "few records of shakespeares private life survive and there has been considerable speculation about such matters as his physical appearance sexuality religious beliefs and whether the works attributed to him were written by others"]

main :: IO ()
main = do
  g <- getStdGen
  print $ unwords $ readChain tree g
  where digested = fmap (addHeadAndTail Nothing Nothing . fmap Just . words) text
        tree = foldl addChain Map.empty digested
