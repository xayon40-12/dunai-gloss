{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

-- | This module contains the elements needed to easily use dunai with Gloss.
--
-- Instead of callbacks used by gloss, a dunai steam function (MSF with Identity as Mando) is used to handle simultaneously time delta and possible events:
-- - the event would be Nothing when Gloss would call update callback
-- - the time delta would be 0 when Gloss would call the event callback
module Dunai.Gloss.Internals
  ( -- * Types
    SFNet,

    -- * Gloss wrappers
    playDunai,

    -- * Utils
    quit,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Data.MonadicStreamFunction (MSF, returnA)
import Data.MonadicStreamFunction.InternalCore (unMSF)
import Graphics.Gloss (Color, Display, Picture (Blank))
import Graphics.Gloss.Interface.Pure.Game (Event, play)
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

-- * Types

-- | Arrow type to describe stream funcion network
type SF a b = MSF Identity a b

-- | Stream function to describe a network that takes time delta (possibly 0) and a possible event as input, and produces a Picture as output
type SFNet = SF (Float, Maybe Event) Picture

-- | The state that cycle into gloss play function.
type State = (Picture, SFNet)

-- * Gloss wrappers

-- | Execute the network for one step.
step :: (i -> (Float, Maybe Event)) -> i -> State -> State
step toGI i (_, sf) = runIdentity $ unMSF sf (toGI i)

-- | Play wrapper that takes no state and only one network instead of the usual gloss callbacks.
playDunai ::
  -- | Disply mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Number of steps to take for each second of real time.
  Int ->
  -- | Arrow network
  SFNet ->
  IO ()
playDunai display color freq network =
  play
    display
    color
    freq
    (Blank, network)
    fst
    (step ((0,) . Just))
    (step (,Nothing))

-- * Utils

{-# NOINLINE quit #-}
quit = unsafePerformIO exitSuccess
