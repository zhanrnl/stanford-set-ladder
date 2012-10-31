{-# LANGUAGE OverloadedStrings #-}

module Ratings where

type Score = Int
type Result = Float
type Rating = Int

-- | Result for player A calculated from a game, uses in-out quadratic
-- easing function
calcResult :: Score -> Score -> Result
calcResult scoreA scoreB =
  let margin = (fromIntegral scoreA) / (fromIntegral (scoreA + scoreB))
      ease x
        | x < 0.5 = 0.5 * ((2 * x) ** 2)
        | x >= 0.5 = 1 - 0.5 * ((2 * (1 - x)) ** 2)
  in ease margin

-- | Using Elo
predictResult :: Rating -> Rating -> Result
predictResult ratingA ratingB =
  let diff = fromIntegral $ ratingB - ratingA
  in 1 / (1 + (10 ** (diff / 400)))

-- | Gives new ratings for a pair of players that just played a 1v1 game 
-- with a given score.
updateRatings :: (Score, Score) -> (Rating, Rating) -> (Rating, Rating)
updateRatings (scoreA, scoreB) (ratingA, ratingB) =
  let predictionA = predictResult ratingA ratingB
      resultA = calcResult scoreA scoreB
      adjustment = round $ kValue * (resultA - predictionA)
      ratingA' = ratingA + adjustment
      ratingB' = ratingB - adjustment
  in (ratingA', ratingB')
  where kValue = 48