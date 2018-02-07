module Lib where

import Data.List(sort, sortBy, group)
import Data.Function(on)

run :: IO ()
run = do src <- readFile "file.in"
         writeFile "file.out" (operate src)

operate :: String -> String
operate text = unlines $ map (\(x,y) -> x ++ " " ++ (show y)) $ 
               sortBy (flip compare `on` snd) $ count_words $ sort $ dropUncertainty $ lines text 

dropUncertainty :: [String] -> [String]
dropUncertainty text = map (takeWhile (\x -> x /= '?')) text  

count_words :: [String] -> [(String, Int)]
count_words list = map (\x -> (head x, length x)) (group list)