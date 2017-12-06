module Day5 where

import Control.Applicative ((<$>))
import Data.Array.IO

getOffsets :: IO [Int]
getOffsets = map read . lines <$> readFile "day5.txt"

{----------------------------------------------------------------------------------------------------{
                                                                      | Puzzle 1
}----------------------------------------------------------------------------------------------------}

-- countJumps1 0 0 <$> getOffsets
countJumps1 :: Int -> Int -> [Int] -> (Int, [Int])
countJumps1 jumpsMade nextPosition offsets =
	let
		(xs, ys) = splitAt nextPosition offsets
	in
		case ys of
			[] -> (jumpsMade, xs)
			(z:zs) -> countJumps1 (jumpsMade + 1) (nextPosition + z) $ concat [xs, (z + 1) : zs]
-- 325922

{----------------------------------------------------------------------------------------------------{
                                                                      | Puzzle 2
}----------------------------------------------------------------------------------------------------}

-- switched to using arrays because lists were taking too long, but I still run out of memory :-(
countJumps2 :: IO Int
countJumps2 = do
	xs <- getOffsets
	xs' <- newListArray (0, (length xs) - 1) xs
	countJumps2' 0 0 xs'

countJumps2' :: Int -> Int -> IOArray Int Int -> IO Int
countJumps2' jumpsMade nextPosition offsets = do
	upperBounds <- snd <$> getBounds offsets
	case nextPosition > upperBounds of
		True -> return jumpsMade
		False -> do
			currentVal <- readArray offsets nextPosition
			writeArray offsets nextPosition $ if currentVal >= 3 then currentVal - 1 else currentVal + 1
			countJumps2' (jumpsMade + 1) (nextPosition + currentVal) offsets
