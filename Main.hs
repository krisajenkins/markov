module Main where
import           Control.Applicative
import qualified Data.Map as Map
import           Data.Monoid
import           System.Random

------------------------------------------------------------
-- General functions.
------------------------------------------------------------

-- |Adds items to the beginning & end of a list.
addHeadAndTail :: a -> a -> [a] -> [a]
addHeadAndTail h t xs = [h] <> xs <> [t]

oneOf :: RandomGen r => ([a], r) -> Maybe (a,r)
oneOf ([], _) = Nothing
oneOf (xs, r) = Just ((!!) xs n, r')
  where (n,r') = randomR (0, length xs - 1) r

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

------------------------------------------------------------
-- Types.
------------------------------------------------------------

-- data Node a = Node a | Start | End deriving (Eq, Ord, Show)
data Node a = Node a deriving (Eq, Ord, Show)
data From a = From (Node a) | Start deriving (Eq, Ord, Show)
data To a = To (Node a) | End deriving (Eq, Ord, Show)
type Edge a = (From a, To a)
type Graph a = Map.Map (From a) [To a]

------------------------------------------------------------
-- Build the chains.
------------------------------------------------------------

edges :: [Node a] -> [Edge a]
edges xs = zip (fmap From xs) (fmap To (tail xs))

addPair :: Ord a => Graph a -> Edge a -> Graph a
addPair t (x,y) = Map.insert x (y:Map.findWithDefault [] x t) t

addChain :: Ord a => Graph a -> [Node a] -> Graph a
addChain t xs = foldl addPair t . addHeadAndTail (Start, To (head xs)) (From (last xs), End) $ edges xs

lineToSentence :: String -> [Node String]
lineToSentence = fmap Node . words

------------------------------------------------------------
-- Read the chains.
------------------------------------------------------------

anyTo :: (RandomGen r, Ord a) => Graph a -> (From a, r) -> Maybe (To a, r)
anyTo g (f, r) = do
                  ts <- Map.lookup f g
                  oneOf (ts, r)

asList :: From a -> [a]
asList Start = []
asList (From (Node a)) = [a]

readChain :: (Ord a, RandomGen r) => Graph a -> From a -> r -> [a]
readChain g f r =
  case anyTo g (f, r) of
       Just (To a, r') -> asList f `mappend` readChain g (From a) r'
       Just (End, _) -> asList f
       Nothing -> asList f

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = do
  r <- getStdGen
  text <- readFile "corpus.txt"
  let sentences = lineToSentence <$> lines text
      tree = foldl addChain Map.empty sentences
  print . unwords $ readChain tree Start r
