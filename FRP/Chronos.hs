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
    -- * Behavior
  , Behavior(..)
  , behave
    -- * Timeline
  , Timeline
  , timeline
  , commute
  ) where

import Data.List ( partition, sort )

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

data Behavior t s
  = Discrete (s -> s)
  | Continuous (t -> s -> s)

newtype Line t s = Line [Signal t s] deriving (Eq,Ord)

data Timeline t s = Timeline {
    _timeLines :: [Line t s]
  , _rea       :: s
  }

timeline :: (Ord t) => [Line t s] -> s -> Timeline t s
timeline l r = Timeline (sort l) r

behave :: t -> Behavior t s -> s -> s
behave t b s = case b of
    Discrete   f -> f s
    Continuous f -> f t s

safeLast :: [a] -> [a]
safeLast s = case s of
    [] -> []
    _  -> [last s]

commute :: (Ord t) => Timeline t s -> t -> Timeline t s
commute (Timeline tl r) t = Timeline tl' r'
  where
    relined = map (reline t . signals t) tl
    tl'     = map fst relined
    r'      = foldl (flip (.)) id (concatMap snd relined) r

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

test :: Timeline Int (String,Int)
test = timeline l ("",0)
  where
    l = map Line
        [
          [Signal 2 . Discrete $ id,Signal 7 $ Discrete $ \(a,x) -> (a,x+9),Signal 9 . Continuous $ \t (a,x) -> (a,x + t)]
        ]
