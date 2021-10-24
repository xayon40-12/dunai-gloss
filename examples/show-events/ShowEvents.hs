{-# LANGUAGE Arrows #-}

-- | Simple example that write the last event on the screen. Usefull to know on what to patern match.
module Main where

import Control.Applicative (Alternative ((<|>)))
import Dunai.Gloss

main :: IO ()
main = playDunai (InWindow "Show events" (800, 600) (100, 100)) white 60 network
  where
    network = proc (dt, e) -> do
      last <- accumulateWith (<|>) Nothing -< e
      returnA -< maybe Blank (Translate (-350) 0 . Scale 0.1 0.1 . Text . show) last
