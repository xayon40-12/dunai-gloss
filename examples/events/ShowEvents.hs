{-# LANGUAGE Arrows #-}

-- | Simple example that write the last event on the screen. Usefull to know on what to patern match.
module Main where

import Dunai.Gloss

main :: IO ()
main = playDunai (InWindow "MSF" (800, 600) (100, 100)) white 60 event update
  where
    event = proc e -> do
      render -< Translate (-350) 0 $ Scale 0.1 0.1 $ Text (show e)
    update = unused
