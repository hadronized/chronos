-------------------------------------------------------------
-- |
-- Module      : FRP.Chronos
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD-style (see the file LICENSE)
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------

module FRP.Chronos (
    -- * Signal
    Signal(..)
  , isDiscrete
  , isContinuous
  , discrete
  , continuous
    -- * Behavior
  , Behavior(..)
  , behave
    -- * Timeline
  , Timeline
  , Line(..)
  , timeline
  , commute
  ) where

import Data.List ( partition, sort )

-- |A `Signal t s` holds a behavior that occurs at a given `t` time and act on
-- a value of type `s`. See `Behavior` for further details.
data Signal t s = Signal t (Behavior t s)

instance (Eq t) => Eq (Signal t s) where
  Signal t0 _ == Signal t1 _ = t0 == t1

instance (Ord t) => Ord (Signal t s) where
  Signal t0 _ `compare` Signal t1 _ = compare t0 t1

isDiscrete :: Signal t s -> Bool
isDiscrete (Signal _ b) = case b of
    Discrete{} -> True
    _          -> False

isContinuous :: Signal t s -> Bool
isContinuous (Signal _ b) = case b of
    Continuous{} -> True
    _            -> False

-- |A `Behavior t s` represents an endomorphism of `s` with the concept of time
-- (defined by the `t` type).
--
-- If the behavior is discrete, the behavior is invoked only once, so no time
-- information is needed.
--
-- If the behavior is continuous, the behavior is invoked continuously, passing
-- the time around.
data Behavior t s
  = Discrete (s -> s)
  | Continuous (t -> s -> s)

-- |Unwrap a function from a behavior and apply it.
behave :: t -> Behavior t s -> s -> s
behave t b s = case b of
    Discrete   f -> f s
    Continuous f -> f t s

-- |Build a discrete signal.
discrete :: t -> (s -> s) -> Signal t s
discrete t f = Signal t (Discrete f)

-- |Build a continuous signal.
continuous :: t -> (t -> s -> s) -> Signal t s
continuous t f = Signal t (Continuous f)

-- |A line is an entry in a timeline. It’s some kind of helper used to group
-- signals that act on the same thing or do similar thing. You can imagine
-- building a line that hosts several discrete same signals at different times.
newtype Line t s = Line [Signal t s] deriving (Eq,Ord)

-- |A timeline gathers lines in order to build a complete time-reactive
-- environnement.
--
-- You can’t directly construct a `Timeline t s`. See the `timeline` function
-- for such a purpose.
newtype Timeline t s = Timeline [Line t s]

-- |Build a timeline from lines.
timeline :: (Ord t) => [Line t s] -> Timeline t s
timeline = Timeline . sort

-- |Commute a line, pulsating/killing signals.
commute :: (Ord t) => Timeline t s -> t -> (s -> s,Timeline t s)
commute (Timeline tl) t = (f,Timeline tl')
  where
    relined = map (reline t . signals t) tl
    tl'     = map fst relined
    f       = foldl (flip (.)) id (concatMap snd relined)

signals :: (Ord t) => t -> Line t s -> ([Signal t s],[Signal t s])
signals t (Line sigs) = span activated sigs
  where
    activated (Signal st _) = t >= st

reline :: t -> ([Signal t s],[Signal t s]) -> (Line t s,[s -> s])
reline t (active,inactive) = (Line $ lastContinuous ++ inactive,behaviors)
  where
    (discrete,continuous)     = partition isDiscrete active
    lastContinuous            = safeLast continuous
    behaviors                 = map signalBehave (lastContinuous ++ discrete)
    signalBehave (Signal _ b) = behave t b

-- Safe last.
safeLast :: [a] -> [a]
safeLast s = case s of
    [] -> []
    _  -> [last s]
