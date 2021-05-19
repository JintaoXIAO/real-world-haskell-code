module Concat where

concat :: [[a]] -> [a]
concat = foldr (++) []
