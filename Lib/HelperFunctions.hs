module Lib.HelperFunctions where

anyTrue :: [Bool] -> Bool
anyTrue []     = False
anyTrue (x:xs) = if x then True else anyTrue xs

compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- TODO: generate a true pseudorandom number
lameRandomizer :: Int -> Int
lameRandomizer seed =
    (iterate (* 255) seed) !! (seed `quot` 5) * ((^) (-1) seed)

unlessDo :: Bool -> (a -> a) -> a -> a
unlessDo condition effect =
    if not condition
        then effect
        else id
