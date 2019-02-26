module Lib.V2 where

type V2 = [Int]

areV2Equal :: V2 -> V2 -> Bool
areV2Equal vA vB =
    (vA !! 0 == vB !! 0) && (vA !! 1 == vB !! 1)

areV2Opposite :: V2 -> V2 -> Bool
areV2Opposite vA vB =
    vA == map (* (-1)) vB
