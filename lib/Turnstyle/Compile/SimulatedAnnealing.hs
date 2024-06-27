{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module Turnstyle.Compile.SimulatedAnnealing
    ( Options (..)
    , defaultOptions

    , run
    ) where

import           System.Random (RandomGen, randomR)

data Options g a = Options
    { oScore       :: a -> Double
    , oNeighbour   :: a -> g -> (a, g)
    , oQuit        :: a -> Bool
    , oTemperature :: Double
    , oCooling     :: Double
    , oGiveUp      :: Maybe Int  -- ^ Restart if no improvement for n iterations
    }

defaultOptions :: Options g a
defaultOptions = Options
    { oScore       = \_ -> 0
    , oNeighbour   = (,)
    , oQuit        = \_ -> False
    , oTemperature = 60
    , oCooling     = 0.9999
    , oGiveUp      = Just 200000
    }

run :: RandomGen g => Options g a -> a -> g -> (a, g)
run Options {..} = \initial gen ->
    let initialScore = oScore initial in
    go oTemperature (initial, initialScore) (initial, initialScore) 0 gen
  where
    go temperature b@(best, bestScore) c@(current, currentScore) !stuck gen0
        | oQuit current                  = (current, gen0)
        | maybe False (stuck >=) oGiveUp = (best, gen0)
        | otherwise =
            let (new, gen1) = oNeighbour current gen0
                !newScore   = oScore new

                (accept, gen2) = p currentScore newScore temperature gen1

                n@(_next, nextScore)
                    | accept    = (new, newScore)
                    | otherwise = c

                (stuck', b')
                    | nextScore > bestScore = (0, n)
                    | otherwise             = (stuck + 1, b) in

            go (temperature * oCooling) b' n stuck' gen2

    p e e' temp gen0
        | e' >= e   = (True, gen0)
        | otherwise =
            let prob = exp (negate (e - e') / temp)
                (rand, gen1) = randomR (0, 1) gen0 in
            (rand < prob, gen1)
