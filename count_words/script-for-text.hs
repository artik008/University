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
          quest_first <- readFile "first"
          quest_second <- readFile "second"
          quest_third <- readFile "third"
          writeFile "resault_f" (operate' fsrc mfsrc (operate quest_first))
          writeFile "resault_s" (operate' fsrc mfsrc (operate quest_second))
          writeFile "resault_t" (operate' fsrc mfsrc (operate quest_third))
          writeFile "idfresault_f" (idfoperate' fsrc mfsrc (operate quest_first))
          writeFile "idfresault_s" (idfoperate' fsrc mfsrc (operate quest_second))
          writeFile "idfresault_t" (idfoperate' fsrc mfsrc (operate quest_third))

operate :: String -> String
operate text = delq text

delq :: String -> String
delq []     = []
delq (t:ts) = case t of
  '{' -> delq ts
  '}' -> delq ts
  '?' -> delq ts
  ',' -> delq ts
  '[' -> delnum ts
  _   -> [t] ++ (delq ts)
  where
    delnum []     = []
    delnum (x:xs) = case x of
      ']' -> delq xs
      _   -> delnum xs

operate' :: String -> String -> String -> String
operate' fs mfs q = concatMap  joinsent  (delzero $ (sortBy (flip compare `on` fst)) $ operate'' fs mfs q)
  where
    joinsent (i, j) = "\n ################################ \n" ++ 
                      (show i) ++ "\n---------------------\n" 
                      ++ j ++ "\n ################################ \n" ++ "\n\n"
    delzero x       = takeWhile nozero x
    nozero (y, _)
      | y == 0.0  = False
      | otherwise = True

idfoperate' :: String -> String -> String -> String
idfoperate' fs mfs q = concatMap  joinsent  (delzero $ (sortBy (flip compare `on` fst)) $ idfoperate'' fs mfs q)
  where
    joinsent (i, j) = (show i) ++ "////" ++ j ++ "\n\n"
    delzero x       = takeWhile nozero x
    nozero (y, _)
      | y == 0.0  = False
      | otherwise = True

operate'' :: String -> String -> String -> [(Double, String)]
operate'' f mf q = zip (getWeigths (words q) (getsent f)) (getsent mf)

idfoperate'' :: String -> String -> String -> [(Double, String)]
idfoperate'' f mf q = zip (getidfWeigths (words q) (getsent f)) (getsent mf)


getsent :: String -> [String]
getsent [] = [] 
getsent x  = [(takeWhile notqu x)] ++ (getsent $ drop 1 (dropWhile notqu x))
  where
    notqu y
      | y == '.'  = False
      | otherwise = True

getWeigths :: [String] -> [String] -> [Double]
getWeigths q l = map (getWeigth q (makeVok ((concatMap words l) ++ q) [])) l

getidfWeigths :: [String] -> [String] -> [Double]
getidfWeigths q l = map (getidfWeigth q vok idf) l
  where
    vok = makeVok ((concatMap words l) ++ q) []
    idf = makeidf vok l

makeidf :: [String] -> [String] -> [Double]
makeidf vok l = map (logidf.(countWinSs l)) vok
  where
    logidf 0            = 0
    logidf n            = log((fromIntegral $ length l)/(fromIntegral n))
    countWinSs [] _     = 0
    countWinSs (x:xs) v = (countW v (words x) 0) + (countWinSs xs v)

makeVok :: [String] -> [String] -> [String]
makeVok [] res     = res
makeVok (l:ls) res = case elem l res of
  True  -> makeVok ls res
  False -> makeVok ls ([l] ++ res)

getWeigth :: [String] -> [String] -> String -> Double
getWeigth q vok l = cosin (makeVect vok q) (makeVect vok (words l))

makeVect :: [String] -> [String] -> [Double]
makeVect vok s = norm $ map (countWord s) vok

getidfWeigth :: [String] -> [String] -> [Double] -> String -> Double
getidfWeigth q vok idf l = cosin (makeidfVect q vok idf) (makeidfVect (words l) vok idf)

makeidfVect :: [String] -> [String] -> [Double] -> [Double]
makeidfVect s vok idf = normidf $ mul idf $ map (countWord s) vok

norm :: [Int] -> [Double]
norm x = map del x
  where
    del y = case len of
      0 -> (fromIntegral y)
      _ -> (fromIntegral y)/len 
    len   =  sqrt (fromIntegral (sum (map (\t -> t*t) x)))

normidf :: [Double] -> [Double]
normidf x = map del x
  where
    del y = case len of
      0 -> y
      _ -> y/len 
    len   =  sqrt (sum (map (\t -> t*t) x))

mul :: [Double] -> [Int] -> [Double]
mul _ [] = [0.0]
mul [] _ = [0.0]
mul (x:xs) (y:ys) = [x*(fromIntegral y)] ++ (mul xs ys) 

countWord :: [String] -> String -> Int
countWord s w = case elem w s of
  True  -> countW w s 0
  False -> 0

countW :: String -> [String] -> Int -> Int
countW _ [] i     = i
countW w (s:ss) i
  | s == w    = countW w ss (i + 1)
  | otherwise = countW w ss i

cosin :: [Double] -> [Double] -> Double
cosin v1 v2 = scal v1 v2
  where
    scal [] [] = 0
    scal _  [] = 0
    scal [] _  = 0
    scal (vx:vxs) (vy:vys) = vx * vy + (scal vxs vys)