module WhyArrows where

import Control.Monad
import Control.Arrow

-- Computation models

-- A computation model determines the ways we can compose computations.
-- By computation model power we mean the number of ways we can compose computations.


-- First, the computation models we all know and love

functorialComputation :: Functor f => i -> f (x, y, z)
functorialComputation i = do
  a <- undefined i
  return $ undefined i a

-- more power
applicativeComputation :: Applicative a => i -> a (x, y, z)
applicativeComputation i = do
  a <- undefined i
  b <- undefined i
  c <- undefined i
  d <- undefined i
  return $ undefined i a b c d

-- more power
monadicComputation :: Monad m => i -> m (x, y, z)
monadicComputation i = do
  a <- undefined i
  b <- undefined i a
  c <- if undefined i a b then undefined i a b else undefined i a b -- choice
  d <- join $ undefined i a b c -- apply
  return $ undefined i a b c d

-- Then, the computation models of arrows

-- same power
arrowApplyChoiceComputation :: (ArrowApply a, ArrowChoice a) => a i (x, y, z)
arrowApplyChoiceComputation = proc i -> do
  a <- undefined -< i
  b <- undefined -< (i, a)
  c <- if undefined i a b then undefined -< (i, a, b) else undefined -< (i, a, b) --choice
  d <- app -< undefined i a b c -- apply
  returnA -< undefined i a b c d

-- less power
arrowChoiceComputation :: ArrowChoice a => a i (x, y, z)
arrowChoiceComputation = proc i -> do
  a <- undefined -< i
  b <- undefined -< (i, a)
  c <- if undefined i a b then undefined -< (i, a, b) else undefined -< (i, a, b) -- choice
  d <- undefined -< undefined i a b c
  returnA -< undefined i a b c d

arrowApplyComputation :: ArrowApply a => a i (x, y, z)
arrowApplyComputation = proc i -> do
  a <- undefined -< i
  b <- undefined -< (i, a)
  c <- undefined -< (i, a, b)
  d <- app -< undefined i a b c -- apply
  returnA -< undefined i a b c d

-- less power
arrowComputation :: Arrow a => a i (x, y, z)
arrowComputation = proc i -> do
  a <- undefined -< i
  b <- undefined -< (i, a)
  c <- undefined -< (i, a, b)
  d <- undefined -< (i, a, b, c)
  returnA -< undefined i a b c d

-- less power is Applicative
-- less power is Functor
