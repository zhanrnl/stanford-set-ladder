{-# LANGUAGE OverloadedStrings #-}

module SetHelpers where

import Control.Monad
import System.Random
import Control.Applicative ((<$>))

data Number  = One   | Two     | Three    deriving (Show, Eq, Enum, Bounded)
data Color   = Red   | Green   | Purple   deriving (Show, Eq, Enum, Bounded)
data Shading = Solid | Shaded  | Hollow   deriving (Show, Eq, Enum, Bounded)
data Shape   = Oval  | Diamond | Squiggle deriving (Show, Eq, Enum, Bounded)
data Card    = Card {cardNumber :: Number,
                     cardColor :: Color,
                     cardShading :: Shading,
                     cardShape :: Shape} deriving (Eq)
instance Show Card where
  show card = "Card" ++
              (show $ cardNumber card) ++
              (show $ cardColor card) ++
              (show $ cardShading card) ++
              (show $ cardShape card)
                                                  
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

allCards :: [Card]
allCards = [Card number color shading shape |
            number <- allValues :: [Number],
            color <- allValues :: [Color],
            shading <- allValues :: [Shading],
            shape <- allValues :: [Shape]]

allDifferentOrAllSame :: Eq a => (a, a, a) -> Bool
allDifferentOrAllSame (x1, x2, x3) =
  (x1 == x2 && x1 == x3) ||
  (x1 /= x2 && x1 /= x3 && x2 /= x3)

setMap :: (a -> b) -> (a, a, a) -> (b, b, b)
setMap f (a1, a2, a3) = (f a1, f a2, f a3)

isSet :: (Card, Card, Card) -> Bool
isSet cards = 
  let numbers = setMap cardNumber cards
      colors = setMap cardColor cards
      shadings = setMap cardShading cards
      shapes = setMap cardShape cards
  in allDifferentOrAllSame numbers && allDifferentOrAllSame colors &&
     allDifferentOrAllSame shadings && allDifferentOrAllSame shapes

get3Combinations :: [Card] -> [(Card, Card, Card)]
get3Combinations cards =
  let numberedCards = zip cards [1..]
  in [(c1, c2, c3) | (c1, n1) <- numberedCards,
                     (c2, n2) <- drop n1 numberedCards,
                     c3 <- drop n2 cards]

getSets :: [Card] -> [(Card, Card, Card)]
getSets cards = filter isSet $ get3Combinations cards

numSetsInCards :: [Card] -> Int
numSetsInCards = length . getSets

randomNCards :: [Card] -> Int -> IO [(Card, Int)]
randomNCards cards n = addNRandomCards cards n []

addNRandomCards :: [Card] -> Int -> [(Card, Int)] -> IO [(Card, Int)]
addNRandomCards cards n input
  | n == 0 = return input
  | otherwise = do
    randomCard <- pickRandomCard cards (map snd input)
    addNRandomCards cards (n-1) (randomCard:input)

randomRExclude :: Int -> [Int] -> IO Int
randomRExclude max exclude = do
  randInt <- randomRIO (0, max - 1)
  if randInt `elem` exclude
    then randomRExclude max exclude
    else return randInt

pickRandomCard :: [Card] -> [Int] -> IO (Card, Int)
pickRandomCard cards exclude = do
  let numberedCards = zip cards [0..]
  randInt <- randomRExclude (length cards) exclude
  return (cards !! randInt, randInt)

makePuzzle :: IO [(Card, Int)]
makePuzzle = do
  cards <- randomNCards allCards 12
  if numSetsInCards (map fst cards) == 6 then return cards
    else makePuzzle

reconstitutePuzzle :: [Int] -> [(Card, Int)]
reconstitutePuzzle nums =
  let cards = allCards
  in map (\n -> (cards !! n, n)) nums