{-# LANGUAGE Arrows #-}

-- | State Monad example where 3 red circles turn around the cursor the cursor.
module Main where

import Dunai.Gloss

main :: IO ()
main = playDunaiM (InWindow "MSF" (800, 600) (100, 100)) white 60 event update extract
  where
    event = proc e -> case e of
      EventMotion p -> arrM (lift . put) -< p
      _ -> returnA -< ()
    update = proc dt -> do
      t <- sumS -< dt
      (x, y) <- constM (lift get) -< ()
      let r = 30
      render -< Pictures [Translate (x + r * cos (t + i * 2 * pi / 3)) (y + r * sin (t + i * 2 * pi / 3)) $ Color red $ ThickCircle 0 10 | i <- [0 .. 2]]
    extract = flip evalState (1e10, 0)
