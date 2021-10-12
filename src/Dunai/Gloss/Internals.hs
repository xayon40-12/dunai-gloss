{-# LANGUAGE Arrows #-}
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
    Renderer,
    ContextT,

    -- * Gloss wrappers
    playDunai,
    playDunaiM,

    -- * Utils
    render,
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
import Graphics.Gloss (Color, Display, Picture (Blank))
import Graphics.Gloss.Interface.Pure.Game (Event, play)

-- * Types

-- | Type to store only last rendered Picture.
type Renderer = Last Picture

-- | Transformer used to make the rendered Picture accessible by gloss while permiting to use other monads for dunai.
type ContextT (m :: Type -> Type) = WriterT Renderer m

-- | Differentiat window events and update cases
data GlossInteraction = WindowEvent Event | Update Float

-- | Arrow type to handle gloss events
type EventNetwork (m :: Type -> Type) = MSink (ContextT m) Event

-- | Arrow type to handle gloss updates
type UpdateNetwork (m :: Type -> Type) = MSink (ContextT m) Float

-- | Arrow type to describe the complete network
type MSFNetwork (m :: Type -> Type) = MSink (ContextT m) GlossInteraction

-- * Gloss wrappers

-- | Gloss wrapper that takes two networks (MSF arrows) to handle updates and events coming from Gloss.
playDunai ::
  -- | Disply mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Number of steps to take for each second of real time.
  Int ->
  -- | Network arrow to handle events.
  EventNetwork Identity ->
  -- | Network arrow to handle updates
  UpdateNetwork Identity ->
  IO ()
playDunai display color freq event update = playDunaiM display color freq event update runIdentity

-- | Execute the network for one step.
step :: (Monad m) => GlossInteraction -> MSFNetwork m -> ContextT m (MSFNetwork m)
step i msf = snd <$> unMSF msf i

-- | Produce a unique network from an event network and an update network
makeNetwork :: (Monad m) => EventNetwork m -> UpdateNetwork m -> MSFNetwork m
makeNetwork event update = proc gi -> case gi of
  WindowEvent e -> event -< e
  Update dt -> update -< dt

-- | Same as 'playDunai' to handle a transformer stack. An extra function is needed to extract the stored Picture from the transformer stack.
playDunaiM ::
  (Monad m) =>
  -- | Disply mode.
  Display ->
  -- | Background color.
  Color ->
  -- | Number of steps to take for each second of real time.
  Int ->
  -- | Network arrow to handle events.
  EventNetwork m ->
  -- | Network arrow to handle updates
  UpdateNetwork m ->
  -- | Transformer stack extractor.
  (m Renderer -> Renderer) ->
  IO ()
playDunaiM display color freq event update extract =
  play
    display
    color
    freq
    (return (makeNetwork event update))
    (fromMaybe Blank . getLast . extract . execWriterT)
    (\event r -> r >>= step (WindowEvent event))
    (\dt r -> r >>= step (Update dt))

-- * Utils

-- | Arrow combinator that takes a Picture as input and store it into the context.
render :: (Monad m) => MSink (ContextT m) Picture
render = arrM $ tell . pure

-- | Arrow combinator to be substituted for unused events or updates handlers.
unused :: (Monad m) => MSink (ContextT m) a
unused = proc _ -> returnA -< ()
