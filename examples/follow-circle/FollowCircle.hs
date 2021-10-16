{-# LANGUAGE Arrows #-}

-- | State Monad example where 3 red circles turn around the cursor the cursor.
module Main where

import Dunai.Gloss
import System.Exit
import System.IO.Unsafe

main :: IO ()
main = playDunai (InWindow "MSF" (800, 600) (100, 100)) white 60 ((1e10, 0), 0) event update draw
  where
    event = proc (w@(_, t), e) -> case e of
      EventMotion p -> returnA -< (p, t)
      EventKey (Char 'q') Down _ _ -> returnA -< unsafePerformIO exitSuccess
      _ -> returnA -< w
    update = proc ((p, t), dt) -> returnA -< (p, t + dt)
    draw ((x, y), t) =
      let r = 30
       in Pictures [Translate (x + r * cos (t + i * 2 * pi / 3)) (y + r * sin (t + i * 2 * pi / 3)) $ Color red $ ThickCircle 0 10 | i <- [0 .. 2]]
