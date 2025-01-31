module Date (Date(..)) where

data Date = Date {
    day :: Int,
    month :: Int,
    year :: Int
} deriving (Show)