#!/usr/bin/runghc

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List.Split
import Text.Regex.PCRE

type Pt    = (Integer, Integer)
type PtHdg = (Integer, Integer, Char)

nhdg :: Char -> Char -> Char -- new heading - heading after the turn
nhdg 'N' 'L' = 'W'
nhdg 'N' 'R' = 'E'
nhdg 'E' 'L' = 'N'
nhdg 'E' 'R' = 'S'
nhdg 'S' 'L' = 'E'
nhdg 'S' 'R' = 'W'
nhdg 'W' 'L' = 'S'
nhdg 'W' 'R' = 'N'

npos :: PtHdg -> Integer -> Pt
npos (x, y, 'N') dis = (x + dis, y) -- new position - distance added
npos (x, y, 'E') dis = (x, y + dis) --  after the turn to the new 
npos (x, y, 'S') dis = (x - dis, y) --  heading
npos (x, y, 'W') dis = (x, y - dis)

loop :: PtHdg -> [[String]] -> PtHdg
loop (x, y, hdg) [] = (x, y, hdg) -- x, y, heading
loop (x, y, hdg) (s:ss) = let dis = read (s !! 1) :: Integer
                              turn = (s !! 0) !! 0 :: Char
                              hdg' = nhdg hdg turn
                              (x', y') = npos (x, y, hdg') dis

                           in loop (x', y', hdg') ss

mdis :: PtHdg -> Integer -- Manhattan distance 
mdis (x, y, _) = abs x + abs y
                                    

main :: IO ()
main = do xs <- readFile "aoc_2016-1.dat"
          let s   = splitOn ", " xs
              reg = "(^[LR])(\\d+)" -- Perl regex
              md  = mdis $ loop (0, 0, 'N') $ map (\x -> drop 1 $ (x =~ reg) !! 0) s
          putStrLn $ "Manhattan distance to Easter Bunny headquarters: " ++ show md ++ " blocks"
         
