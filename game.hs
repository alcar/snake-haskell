{-# LANGUAGE TemplateHaskell #-}

import Lib.HelperFunctions
import Lib.Snake
import Lib.V2
import Lib.World

import Control.Lens
import Data.Maybe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Prelude hiding (head)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- CONSTANTS

k_WINDOW_SIDE_LENGTH :: Int
k_WINDOW_SIDE_LENGTH = 400

k_PAD_UP :: V2
k_PAD_UP = [0, 1]

k_PAD_LEFT :: V2
k_PAD_LEFT = [-1, 0]

k_PAD_DOWN :: V2
k_PAD_DOWN = [0, -1]

k_PAD_RIGHT :: V2
k_PAD_RIGHT = [1, 0]

k_HEAD_SIDE_LENGTH :: Float
k_HEAD_SIDE_LENGTH = fromIntegral k_WINDOW_SIDE_LENGTH / 20.0

k_BODY_UNIT_RADIUS :: Float
k_BODY_UNIT_RADIUS = k_HEAD_SIDE_LENGTH / 2

k_FRUIT_RADIUS :: Float
k_FRUIT_RADIUS = k_HEAD_SIDE_LENGTH / 2

k_SNAKE_SPEED :: Int
k_SNAKE_SPEED = round k_HEAD_SIDE_LENGTH

k_INITIAL_SNAKE :: Snake
k_INITIAL_SNAKE =
    Snake
        [20, 40]
        [[0, 40]]
        [1, 0]

k_INITIAL_FRUIT :: V2
k_INITIAL_FRUIT = [(-k_WINDOW_SIDE_LENGTH) `quot` 4, (-k_WINDOW_SIDE_LENGTH) `quot` 4]

k_INITIAL_WORLD :: World
k_INITIAL_WORLD =
    World
        0
        Set.empty
        k_INITIAL_SNAKE
        k_INITIAL_FRUIT


-- DRAWING
-- Draws the world

-- . Creates the picture list based on the snake's body
createPictureList :: [V2] -> [Picture]
createPictureList []     = []
createPictureList (v:vs) =
    Translate
        (vX + k_BODY_UNIT_RADIUS)
        (vY + k_BODY_UNIT_RADIUS)
        (circleSolid k_BODY_UNIT_RADIUS)
    : createPictureList vs
    where
        vX = fromIntegral (v !! 0) :: Float
        vY = fromIntegral (v !! 1) :: Float

-- . The draw function itself
draw :: World -> Picture
draw w =
    pictures $
        Translate
            ((snakeHead 0) + k_HEAD_SIDE_LENGTH / 2)
            ((snakeHead 1) + k_HEAD_SIDE_LENGTH / 2)
            (rectangleSolid k_HEAD_SIDE_LENGTH k_HEAD_SIDE_LENGTH)
        : Translate
            ((snakeFruit 0) + k_FRUIT_RADIUS)
            ((snakeFruit 1) + k_FRUIT_RADIUS)
            (color red (circleSolid k_FRUIT_RADIUS))
        : createPictureList (w ^. snake ^. body)
        where
            snakeHead  x = fromIntegral ((w ^. snake ^. head) !! x) :: Float
            snakeFruit x = fromIntegral ((w ^. fruit) !! 0) :: Float


-- INPUT HANDLING
-- Updates the world after certain inputs

input :: Event -> World -> World
input (EventKey key Up   _ _)   = keyDown %~ Set.delete key
input (EventKey key Down _ _)   =
    (keyDown %~ Set.insert key) . (Map.findWithDefault id key (Map.fromList [
        (Char 'w'           , checkPadChange k_PAD_UP   ),
        (Char 'a'           , checkPadChange k_PAD_LEFT ),
        (Char 's'           , checkPadChange k_PAD_DOWN ),
        (Char 'd'           , checkPadChange k_PAD_RIGHT),
        (SpecialKey KeyUp   , checkPadChange k_PAD_UP   ),
        (SpecialKey KeyLeft , checkPadChange k_PAD_LEFT ),
        (SpecialKey KeyDown , checkPadChange k_PAD_DOWN ),
        (SpecialKey KeyRight, checkPadChange k_PAD_RIGHT)]))
input _                         = id


-- STATE HANDLING
-- Advances the world to the next state each frame

-- . Checks the next state
checkNextState :: World -> World
checkNextState w =
    if anyTrue (map (areV2Equal (w ^. snake ^. head)) (w ^. snake ^. body))
        then k_INITIAL_WORLD
        else w &
            if (w ^. snake ^. head) == (w ^. fruit)
                then
                    generateNewFruit k_WINDOW_SIDE_LENGTH
                    . updateSnakeWithFruit k_WINDOW_SIDE_LENGTH k_SNAKE_SPEED
                else
                    updateSnakeWithoutFruit k_WINDOW_SIDE_LENGTH k_SNAKE_SPEED



-- TODO: create new method to check if head and last body are perpendicular to avoid hit bug

-- . The step function itself
step :: Float -> World -> World
step dt w =
    trace
        (show (w ^. fruit, w ^. snake ^. head))
        (w &
            (time +~ dt)
            . checkNextState
        )


-- MAIN MODULE
-- Where the magic happens

main :: IO ()
main =
    play
        (InWindow "snake game" (k_WINDOW_SIDE_LENGTH, k_WINDOW_SIDE_LENGTH) (100, 100))
        white
        15
        k_INITIAL_WORLD
        draw
        input
        step
