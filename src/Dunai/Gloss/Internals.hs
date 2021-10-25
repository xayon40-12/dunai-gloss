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
    playDunaiIO,

    -- * Utils
    quit,
    unsafeQuit,
    repeatMSF,
    repeatMStream,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (second)
import Data.Kind (Type)
import Data.MonadicStreamFunction (MSF, returnA)
import Data.MonadicStreamFunction.InternalCore (MSF (..), unMSF)
import Graphics.Gloss (Color, Display, Picture (Blank))
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Pure.Game (Event, play)
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

-- * Types

-- | Arrow type to describe stream funcion network
type SF a b = MSF Identity a b

-- | Monadic Stream function to describe a network that takes time delta (possibly 0) and a possible event as input, and produces a Picture as output
type MSFNet m = MSF m (Float, Maybe Event) Picture

-- | The state that cycle into gloss play function.
type MState m = (Picture, MSFNet m)

-- | Stream function to describe a network that takes time delta (possibly 0) and a possible event as input, and produces a Picture as output
type SFNet = MSFNet Identity

-- | The state that cycle into gloss play function.
type State = MState Identity

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

-- | Execute the network for one step.
mstep :: (Monad m) => (i -> (Float, Maybe Event)) -> i -> MState m -> m (MState m)
mstep toGI i (_, sf) = unMSF sf (toGI i)

-- | Play wrapper that takes no state and only one network instead of the usual gloss callbacks.
playDunaiIO ::
  -- | Disply mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Number of steps to take for each second of real time.
  Int ->
  -- | Arrow network
  MSFNet IO ->
  IO ()
playDunaiIO display color freq network =
  playIO
    display
    color
    freq
    (Blank, network)
    (return . fst)
    (mstep ((0,) . Just))
    (mstep (,Nothing))

-- * Utils

-- | Quit the pregram in place (to use in 'playDunaiIO').
quit :: IO ()
quit = exitSuccess

-- | Quit the pregram in place by using 'unsafePerformIO'.
{-# NOINLINE unsafeQuit #-}
unsafeQuit = unsafePerformIO exitSuccess

-- | Repeat n times an MSF at each call.
-- 'n' must be greater or equals to 0.
-- __Examples:__
-- @
-- repeatMSF 3 (arr (*2)) = arr (*2^3)
-- @
repeatMSF :: (Monad m) => Int -> MSF m a a -> MSF m a a
repeatMSF n' msf' = MSF $ \a -> go n' (a, msf')
  where
    go 0 (a, msf) = return (a, repeatMSF n' msf)
    go n (a, msf) = go (n -1) =<< unMSF msf a

-- | Repeat n times an MStream at each call, disregarding itermediate output.
-- 'n' must be greater or equals to 1.
-- __Example:__
-- @
-- repeatMStream 3 count = count >>> arr (*3)
-- @
repeatMStream :: (Monad m) => Int -> MSF m () b -> MSF m () b
repeatMStream n' msf' = MSF $ \_ -> go n' msf'
  where
    go 1 msf = second (repeatMStream n') <$> unMSF msf ()
    go n msf = go (n -1) . snd =<< unMSF msf ()
