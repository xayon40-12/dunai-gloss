{-# LANGUAGE Arrows #-}

-- | Simple example that stores the keys pressed in a Set, and remove them if they are pressed again.
module Main where

import Data.Set
import Dunai.Gloss

main :: IO ()
main = playDunai (InWindow "Key pressed" (800, 600) (100, 100)) white 60 network
  where
    network = proc (dt, e) -> do
      last <- accumulateWith store empty -< e
      returnA -< (Translate (-350) 0 . Scale 0.1 0.1 . Text . show) last
    store (Just (EventKey k Down _ _)) a = if member k a then delete k a else insert k a
    store _ a = a
