{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Graphics.Gloss
import Prelude hiding (head)

type V2 = [Int]

data Snake = Snake
    { _head      :: V2
    , _body      :: [V2]
    , _direction :: V2
    , _fruit     :: V2
    } deriving (Show)

makeLenses ''Snake

windowSideLength :: Int
windowSideLength = 50

-- splice :: Int -> Int -> [a] -> [a] -> [a]
-- splice i nDrop newPart list =
--     take i list ++ newPart ++ drop (i + nDrop) list

-- insertAt :: Int -> a -> [a] -> [a]
-- insertAt i x =
--     splice i 0 [x]

anyTrue :: [Bool] -> Bool
anyTrue (x:xs) = if x then True else anyTrue xs
anyTrue _      = False

areV2Equal :: V2 -> V2 -> Bool
areV2Equal vA vB =
    (vA !! 0 == vB !! 0) && (vA !! 1 == vB !! 1)

randomizer :: Int -> Int
randomizer seed =
    12345 * seed `mod` windowSideLength

generateNewFruit :: Snake -> Snake
generateNewFruit snake =
    snake & fruit .~ [randomNumberA, randomNumberB]
    where
        randomNumberA = randomizer seed
        seed          = (snake ^. body) !! 0 !! 1
        randomNumberB = randomizer (randomNumberA)

incrementBody :: Snake -> Snake
incrementBody snake =
    snake & body %~ (++ [snake ^. head])

decrementBody :: Snake -> Snake
decrementBody =
    body %~ (drop 1)

updateHead :: Snake -> Snake
updateHead snake =
    snake & head %~ (map (`rem` windowSideLength)) . (zipWith (+) $ snake ^. direction)

updateSnake :: Snake -> Snake
updateSnake snake =
    if ((snake ^. head) == (snake ^. fruit))
        then snake & generateNewFruit . updateHead . incrementBody
        else snake & decrementBody    . updateHead . incrementBody

stepGame :: Snake -> Snake
stepGame snake =
    if anyTrue (map (areV2Equal (snake ^. head)) (snake ^. body))
       then initialSnake
       else updateSnake snake

initialSnake :: Snake
initialSnake = Snake [1, 2] [[1, 1], [1, 0], [0, 0], [0, 1], [0,2]] [1, 0] [5, 6]

-- Starts @ (1,2)
-- Goes 4 units right, 7 units down, 1 unit right, 1 unit up, 1 unit left
-- Game over, resets
main = do
    print $ "00 - " ++ show initialSnake
    print $ "01 - " ++ show (stepGame initialSnake)
    print $ "02 - " ++ show (stepGame $ stepGame initialSnake)
    print $ "03 - " ++ show (stepGame $ stepGame $ stepGame initialSnake)
    print $ "04 - " ++ show (stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "05 - " ++ show (stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "06 - " ++ show (stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "07 - " ++ show (stepGame $ stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "08 - " ++ show (stepGame $ stepGame $ stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "09 - " ++ show (stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "10 - " ++ show (stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "11 - " ++ show (stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "12 - " ++ show (stepGame $ direction .~ [1, 0] $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "13 - " ++ show (stepGame $ direction .~ [0, -1] $ stepGame $ direction .~ [1, 0] $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "14 - " ++ show (stepGame $ direction .~ [-1, 0] $ stepGame $ direction .~ [0, -1] $ stepGame $ direction .~ [1, 0] $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)
    print $ "15 - " ++ show (stepGame $ stepGame $ direction .~ [-1, 0] $ stepGame $ direction .~ [0, -1] $ stepGame $ direction .~ [1, 0] $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ stepGame $ direction .~ [0, 1] $ stepGame $ stepGame $ stepGame $ stepGame initialSnake)