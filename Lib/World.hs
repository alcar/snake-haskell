{-# LANGUAGE TemplateHaskell #-}

module Lib.World where

import Lib.HelperFunctions
import Lib.Snake
import Lib.V2

import Control.Lens
import Graphics.Gloss.Interface.Pure.Game
import Prelude hiding (head)
import System.Random
import qualified Data.Set as Set

type PressedKeys = Set.Set Key

data World = World
    { _time    :: Float
    , _keyDown :: PressedKeys
    , _snake   :: Snake
    , _fruit   :: V2
    } deriving (Show)
makeLenses ''World

generateNewFruit :: Int -> World -> World
generateNewFruit windowSideLength w =
    w & fruit .~ [randomNumberA, randomNumberB]
    where
        randomNumberA  = (lameRandomizer 12345) `rem` 10 * 20
        randomNumberB  = (lameRandomizer (abs randomNumberA)) `rem` 10 * 20

updateSnakeWithFruit :: Int -> Int -> World -> World
updateSnakeWithFruit windowSideLength speed =
    snake %~
        updateHead windowSideLength speed
        . incrementBody

updateSnakeWithoutFruit :: Int -> Int -> World -> World
updateSnakeWithoutFruit windowSideLength speed =
    snake %~
        decrementBody
        . updateHead windowSideLength speed
        . incrementBody

checkPadChange :: V2 -> World -> World
checkPadChange pad world =
    unlessDo (areV2Opposite (world ^. snake ^. direction) pad) (snake . direction .~ pad) world
