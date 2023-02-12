#!/usr/bin/runghc

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List.Split
import Text.Regex.PCRE

type Pt      = (Integer, Integer)             -- Point
type PtHdg   = (Integer, Integer, Char)       -- Point, Heading
type PtHdgPP = (Integer, Integer, Char, [Pt]) -- Point, Heading, Prev Points

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

loop :: PtHdgPP -> [[String]] -> PtHdgPP
loop (x, y, hdg, prev) [] = (x, y, hdg, prev) -- x, y, heading, previous positions array
loop (x, y, hdg, prev) (s:ss) = do let dis = read (s !! 1) :: Integer
                                   let turn = (s !! 0) !! 0 :: Char -- L or R
                                   let hdg' =  nhdg hdg turn
                                   let (x', y') = npos (x, y, hdg') dis

                                   --if' find (== (x', y')) prev
                                      --   (10000, 10000) : prev 
                                    -- else
                                         
                                   let prev' = (x' , y') : prev
                                    
                                    in loop (x', y', hdg', prev') ss
                                     
--mdis :: (Integer, Integer, Char, [Pt]) -> Integer -- Manhattan distance 
mdis :: PtHdgPP -> Integer -- Manhattan distance 
mdis (x, y, _, _) = abs x + abs y

main :: IO ()
main = do xs <- readFile "aoc_2016-1.dat"
          let s = splitOn ", " xs           --del3
          let reg = "(^[LR])(\\d+)" -- Perl regex
          let (a, b, h, p) = loop (0, 0, 'N', []) (map (\x -> drop 1 ((x =~ reg) !! 0)) s)
          let md  = mdis . loop (0, 0, 'N', []) $ map (\x -> drop 1 $ (x =~ reg) !! 0) s
          print (reverse p)
          print md

          
