{-# LANGUAGE TemplateHaskell #-}

module Lib.Snake where

import Lib.V2

import Control.Lens
import Data.Fixed
import Prelude hiding (head)

data Snake = Snake
    { _head      :: V2
    , _body      :: [V2]
    , _direction :: V2
    } deriving (Show)
makeLenses ''Snake

incrementBody :: Snake -> Snake
incrementBody snake =
    snake & body %~ (++ [snake ^. head])

decrementBody :: Snake -> Snake
decrementBody =
    body %~ (drop 1)

updateHead :: Int -> Int -> Snake -> Snake
updateHead windowSideLength speed snake
  | snakeProjection 0 > maxBoundary = setHead 0 minBoundary
  | snakeProjection 0 < minBoundary = setHead 0 maxBoundary
  | snakeProjection 1 > maxBoundary = setHead 1 minBoundary
  | snakeProjection 1 < minBoundary = setHead 1 maxBoundary
  | otherwise                       = zipWithHead (+) $ map (* speed) (snake ^. direction)
    where
        snakeProjection x = ((snake ^. head) !! x) + (speed * (snake ^. direction) !! x)
        maxBoundary       = round (fromIntegral (windowSideLength * 9) / (fromIntegral 20))
        minBoundary       = round (fromIntegral (windowSideLength * (-1)) / (fromIntegral 2))
        setHead i x       = snake & head . ix i .~ x
        zipWithHead f x   = snake & head %~ zipWith f x
