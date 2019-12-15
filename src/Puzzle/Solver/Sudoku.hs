{-# LANGUAGE BangPatterns #-}

module Puzzle.Solver.Sudoku
    ( Table
    , solve
    ) where

import           Control.Arrow
import           Control.Monad
import           Data.Function (on)
import           Data.List     (minimumBy)


-- |A type for sudoku tables
type Table = [[Int]]

-- |Solves a sudoku puzzle
solve :: Table -> [Table]
solve = filter isCompleted . dfs' tentatives

-- |Strict dfs
dfs' :: (a -> [a]) -> a -> [a]
dfs' f !x = x : (f x >>= dfs' f)

-- |Checks if the given sudoku table is completed
isCompleted :: Table -> Bool
isCompleted = (all . all) (/= 0)

tentatives :: Table -> [Table]
tentatives table = do
    let posWithCands = f table <$> posBlank table
        f t p@(x, y) = (p, candidates x y t)
        ((x', y'), cands) = minimumBy comp posWithCands
        comp = compare `on` length . snd
    guard $ (not . null) posWithCands
    replaceWith2 x' y' table <$> cands

posBlank :: Table -> [(Int, Int)]
posBlank t = do
    (y, r) <- withIndex t
    (x, c) <- withIndex r
    guard $ 0 `elem` r && c == 0
    return (x, y)

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

row :: Int -> [[a]] -> [a]
row y = (!! y)

col :: Int -> [[a]] -> [a]
col x = fmap (!! x)

box :: Int -> Int -> [[a]] -> [a]
box x y t = f y t >>= f x
    where
    f a = take 3 . drop (a `div` 3 * 3)

candidates :: Int -> Int -> Table -> [Int]
candidates x y table = do
    m <- [1..9]
    guard $ m `notElem` join ([row y, col x, box x y] <*> pure table)
    return m

replaceWith :: Int -> [a] -> a -> [a]
replaceWith n l a = splitAt n >>> second tail >>> uncurry (inbetween a) $ l

inbetween :: a -> [a] -> [a] -> [a]
inbetween a h t = h ++ a : t

replaceWith2 :: Int -> Int -> [[a]] -> a -> [[a]]
replaceWith2 x y t a = replaceWith y t l
    where
    l = replaceWith x (t !! y) a
