{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( someFunc
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable

class (Monad m) =>
      Random m where
  rand :: m Double

class Fitness g input fitness where
  measureFitness :: Traversable t => t input -> g -> fitness

class (Ord fitness, Fitness g input fitness) =>
      Individual g input fitness | g -> input, g -> fitness where
  spawn :: Random m => m g
  mutate :: Random m => g -> m g

class (Individual g input fitness, Traversable t) =>
      Population g input fitness t | g -> input, g -> fitness where
  data MatingGroup g :: *
    -- | Select Parents for the next generation
  selection :: (Random m) => t g -> m (t g)
  -- | Group Parents for mating
  groupForMating :: (Random m) => t g -> m (t (MatingGroup g))
  -- | Produce offspring from MatingGroup
  reproduce :: (Random m) => MatingGroup g -> m (t g)

populationFitness ::
     (Population g input fitness t, Traversable t, Traversable s)
  => s input
  -> t g 
  -> t (g, fitness)
populationFitness trainData = fmap (id &&& measureFitness trainData)

generateNewPopulation ::
     (Random m, Population a input fitness t, Monad t, Traversable t)
  => t a
  -> m (t a)
generateNewPopulation =
  fmap join . traverse reproduce <=< groupForMating <=< selection

epoch ::
     (Traversable t, Traversable s, Population g input fitness t, Random m)
  => s input
  -> t g
  -> m (t g)
epoch = undefined

-- TODO Add options to remove redundant values
someFunc :: IO ()
someFunc = putStrLn "someFunc"
