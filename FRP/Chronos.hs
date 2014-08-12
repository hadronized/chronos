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
  , isPulse
  , isContinuous
    -- * Timeline
  , Timeline
  , Line(..)
  , timeline
  , commute
  ) where

import Data.List ( partition, sort )
import Data.Monoid ( (<>) )

-- |A `Signal t s` holds an endormophism that occurs at a given `t` time and
-- acts on a value of type `s`. Signals can be either Pulse or continuous.
--
-- A `Pulse t f` signal is a simple function `s -> s` pulsed at `t`.
--
-- A `Continuous start end ft` signal is a function `t -> s -> s` continuously
-- fed by time `t`. It starts at `start` and vanish at `end`.
data Signal t s
  = Pulse t (s -> s)
  | Continuous t t (t -> s -> s)

instance (Eq t) => Eq (Signal t s) where
  Pulse t0 _ == Pulse t1 _    = t0 == t1
  Continuous s0 e0 _ == Continuous s1 e1 _ = s0 == s1 && e0 == e1

instance (Ord t) => Ord (Signal t s) where
  Pulse t0 _ `compare` Pulse t1 _ = compare t0 t1
  Continuous s0 e0 _ `compare` Continuous s1 e1 _ = compare s0 s1 <> compare e0 e1
  Pulse t _ `compare` Continuous s _ _ = compare t s
  Continuous s _ _ `compare` Pulse t _ = compare t s

isPulse :: Signal t s -> Bool
isPulse Pulse{} = True
isPulse _       = False

isContinuous :: Signal t s -> Bool
isContinuous Continuous{} = True
isContinuous _            = False

-- |Unwrap a function from a signal and apply it without checking time
-- conditions.
behave :: t -> Signal t s -> s -> s
behave t b s = case b of
    Pulse   _ f      -> f s
    Continuous _ _ f -> f t s

-- |A line is an entry in a timeline. It’s some kind of helper used to group
-- signals that act on the same thing or do similar thing. You can imagine
-- building a line that hosts several Pulse same signals at different times.
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
    relined = map (reline t) tl
    tl'     = map fst relined
    f       = foldl (flip (.)) id (concatMap snd relined)

-- |Get active and unactive signals from a `Line`.
signals :: (Ord t) => t -> Line t s -> ([Signal t s],[Signal t s])
signals t (Line sigs) = span activated sigs
  where
    activated sig = case sig of
      Pulse t0 _       -> t >= t0
      Continuous s e _ -> t >= s && t < e

-- |Reconstruct a `Line` according to time.
reline :: (Ord t) => t -> Line t s -> (Line t s,[s -> s])
reline t l = (Line $ lastContinuous ++ inactive,f)
  where
    (active,inactive) = signals t l
    (disc,cont)       = partition isPulse active
    lastContinuous    = safeLast cont
    f                 = map (behave t) (lastContinuous ++ disc)

-- Safe last.
safeLast :: [a] -> [a]
safeLast s = case s of
    [] -> []
    _  -> [last s]
