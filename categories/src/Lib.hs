module Lib
    ( someFunc
    ) where


data Option a = Some a | None
instance Functor Option where
    fmap f (Some x) = Some $ f x
    fmap f None = None

inc :: Int -> Int
inc x = x + 1

mapped :: Option Int -> Option Int
mapped = fmap inc

showOp :: Option Int -> [Char]
showOp (Some x) = show x
showOp None = "None"

someFunc :: IO ()
someFunc = putStrLn . showOp . mapped  $ None
