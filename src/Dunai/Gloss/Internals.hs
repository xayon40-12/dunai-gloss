{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}

-- | This module contains the elements needed to easily use dunai with Gloss.
--
-- Instead of callbacks used by gloss, dunai arrows (MSF) are used to handle events and updates.
--
-- You are expected to render the scene (with the 'render' combinator) somewhere inside the events and updates handlers.
-- The scene produced by 'render' will be presented to the screen the next time gloss would have called the rendering callback.
module Dunai.Gloss.Internals
  ( -- * Types
    UpdateNetwork,
    EventNetwork,

    -- * Gloss wrappers
    playDunai,

    -- * Utils
    unused,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Trans.MSF (WriterT, execWriterT)
import Control.Monad.Trans.MSF.Writer (tell)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.MonadicStreamFunction (MSF, MSink, arrM, returnA)
import Data.MonadicStreamFunction.InternalCore (unMSF)
import Data.Monoid (Last (getLast))
import Debug.Trace (trace)
import Graphics.Gloss (Color, Display, Picture (Blank))
import Graphics.Gloss.Interface.Pure.Game (Event, play)

-- * Types

-- | Differentiat window events and update cases
data GlossInteraction = WindowEvent Event | Update Float

-- | Arrow type to describe stream funcion network
type SF a b = MSF Identity a b

-- | Arrow type to handle gloss events
type EventNetwork a = SF (a, Event) a

-- | Arrow type to handle gloss updates
type UpdateNetwork a = SF (a, Float) a

-- | Arrow type to describe the complete network
type SFNet a = SF (a, GlossInteraction) a

-- * Gloss wrappers

-- | Execute the network for one step.
step :: (i -> GlossInteraction) -> i -> (a, SFNet a) -> (a, SFNet a)
step toGI i (w, sf) = runIdentity $ unMSF sf (w, toGI i)

-- | Produce a unique network from an event network and an update network
makeNetwork :: EventNetwork a -> UpdateNetwork a -> SFNet a
makeNetwork event update = proc (w, gi) -> case gi of
  WindowEvent e -> event -< (w, e)
  Update dt -> update -< (w, dt)

-- | Same as 'playDunai' to handle a transformer stack. An extra function is needed to extract the stored Picture from the transformer stack.
playDunai ::
  -- | Disply mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Number of steps to take for each second of real time.
  Int ->
  -- | State
  a ->
  -- | Network arrow to handle events.
  EventNetwork a ->
  -- | Network arrow to handle updates
  UpdateNetwork a ->
  -- | Network arrow to draw the state
  (a -> Picture) ->
  IO ()
playDunai display color freq world event update draw =
  play
    display
    color
    freq
    (world, makeNetwork event update)
    (draw . fst)
    (step WindowEvent)
    (step Update)

-- * Utils

-- | Arrow combinator to be substituted for unused events or updates handlers.
unused :: SF (a, b) a
unused = proc a -> returnA -< fst a
