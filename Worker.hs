module Worker (Worker(..)) where

data Worker = Worker {
    workerId :: Int,
    name :: String,
    dispo :: Bool,
    skills :: [String]
} deriving (Show)