{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List(sortBy, length)
import Data.Function(on)

main :: IO ()
main = do mfsrc <- readFile "collection"
          fsrc <- readFile "format_collection"
          quest_first <- readFile "q_first"
          quest_second <- readFile "q_second"
          quest_third <- readFile "q_third"
          writeFile "resault_f" (operate fsrc mfsrc (delq quest_first))
          writeFile "resault_s" (operate fsrc mfsrc (delq quest_second))
          writeFile "resault_t" (operate fsrc mfsrc (delq quest_third))

delq :: String -> String
delq []     = []
delq (t:ts) = case t of
  '{' -> delq ts
  '}' -> delsp ts
  '?' -> delq ts
  ',' -> delq ts
  '[' -> delnum ts
  _   -> [t] ++ (delq ts)
  where
    delnum []     = []
    delnum (x:xs) = case x of
      ']' -> delq xs
      _   -> delnum xs
    delsp []      = []
    delsp (y:ys)
      | y == '{'  = " " ++ (delq ys)
      | otherwise = delq (y:ys)

operate :: String -> String -> String -> String
operate fs mfs q = concatMap  joinsent  (delzero $ (sortBy (flip compare `on` fst)) $ operate' fs mfs q)
  where
    joinsent (i, j) = "\n------------------------------------\n" 
                      ++ j  ++ "\n------------------------------------\n" ++ 
                      (show i) ++ "\n------------------------------------\n" ++ "\n\n"
    delzero x       = takeWhile nozero x
    nozero (y, _)
      | y == 0.0  = False
      | otherwise = True

operate' :: String -> String -> String -> [(Double, String)]
operate' f mf q = zip (getWeigths (words q) (map words $ getsent f)) (getsent mf)

getsent :: String -> [String]
getsent [] = [] 
getsent x  = [(takeWhile notqu x)] ++ (getsent $ drop 1 (dropWhile notqu x))
  where
    notqu y
      | y == '.'  = False
      | otherwise = True

getWeigths :: [String] -> [[String]] -> [Double]
getWeigths q l = map (getWeigth (q_ver q l) q) l

q_ver :: [String] -> [[String]] -> [Double]
q_ver q vok = map (\x -> (word_in_all x)/allwords) q
  where
    allwords = case ln of
      0 -> 10000000000
      _ -> ln
    ln = fromIntegral $ sum $ map length vok
    word_in_all word = case winall word of
      0 -> 1.0
      _ -> fromIntegral $ winall word
    winall x = sum (map (countWord x) vok)

getWeigth :: [Double] -> [String] -> [String] -> Double
getWeigth pt q s = get_ver pt (q_one_ver q s)

q_one_ver :: [String] -> [String] -> [Double]
q_one_ver q s = map (\x -> (fromIntegral $ countWord x s)/allwords) q
  where
    allwords = case ln of
      0 -> 10000000000
      _ -> ln
    ln = fromIntegral $ length s


get_ver :: [Double] -> [Double] -> Double
get_ver [] _ = 1
get_ver _ [] = 1
get_ver (x:xs) (y:ys) = (one_word x y)*(get_ver xs ys)
  where
    one_word pt ptM = (1 - lambda)*pt + lambda*ptM 

countWord :: String -> [String] -> Int
countWord w s = case elem w s of
  True  -> countW w s 0
  False -> 0

countW :: String -> [String] -> Int -> Int
countW _ [] i     = i
countW w (s:ss) i
  | s == w    = countW w ss (i + 1)
  | otherwise = countW w ss i

lambda :: Double
lambda = 0.5