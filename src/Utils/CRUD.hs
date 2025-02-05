module Utils.CRUD where

import qualified Data.Set as Set
import Data.Set (Set)

-- Crear un conjunto vacío
emptySet :: Ord a => Set a
emptySet = Set.empty

-- Añadir elementos al conjunto
addElement :: Ord a => a -> Set a -> Set a
addElement element set = Set.insert element set

-- Eliminar un elemento del conjunto
removeElement :: Ord a => a -> Set a -> Set a
removeElement element set = Set.delete element set

-- Buscar un elemento en el conjunto
findElement :: Ord a => a -> Set a -> Bool
findElement element set = Set.member element set