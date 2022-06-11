{-# LANGUAGE Arrows #-}

-- | Compute the value of pi by generating random number. Using o Storable vector thanks to Dunai MSF that can handle IO.
module Main where

import Data.Bool (bool)
import Data.Complex
import Data.Vector.Storable.Mutable
import Data.Word
import Dunai.Gloss
import Prelude hiding (read, replicate)

main :: IO ()
main = do
  bitmap <- replicate (s * s * 4) 255
  field <- generate (s * s) init
  field2 <- replicate (s * s) 0
  playDunaiIO (InWindow "Random Pi" (s, s) (100, 100)) white 30 (network field field2 bitmap)
  where
    init :: Int -> Complex Double
    init i =
      let x' = mod i s
          x = fromIntegral x' * dx
          y' = div i s
          y = fromIntegral y' * dx
          x0 = l / 5
          y0 = l / 2
          kx = l
          ky = 0
          r = l / 20
       in mkPolar (exp (- ((x - x0) ^ 2 + (y - y0) ^ 2) / r)) (kx * x + ky * y)

s :: Int
s = 200

l, dx, dt :: Double
l = 10
dx = l / fromIntegral s
dt = 3e-3

ii :: Complex Double
ii = 0 :+ 1

toRGBA ptr = bitmapOfForeignPtr s s (BitmapFormat TopToBottom PxRGBA) ptr False

network field field2 bitmap = proc (upd, _) -> do
  if upd > 0
    then do
      i <- count -< ()
      let (cur, nex) = if odd i then (field, field2) else (field2, field)
      arrM (uncurry update) -< (cur, nex)
      arrM (`render` bitmap) -< nex
    else returnA -< ()
  returnA -< img
  where
    img = toRGBA . fst $ unsafeToForeignPtr0 bitmap

update field nfield = go 0
  where
    dir x y
      | x < 0 = dir 0 y
      | x >= s = dir (s -1) y
      | y < 0 = dir x 0
      | y >= s = dir x (s -1)
      | otherwise = x + s * y
    go i
      | i >= s * s = return ()
      | otherwise = do
        let x = mod i s
            y = div i s
        c <- read field i
        cu <- read field (dir x (y -1))
        cd <- read field (dir x (y + 1))
        cl <- read field (dir (x -1) y)
        cr <- read field (dir (x + 1) y)
        let v = c + (dt :+ 0) * hv x y c cu cd cl cr
        write nfield i v
        go (i + 1)

hv :: Int -> Int -> Complex Double -> Complex Double -> Complex Double -> Complex Double -> Complex Double -> Complex Double
hv x y c cu cd cl cr = - ii * (- a * (cu + cd + cl + cr -4 * c))
  where
    a = 1 / dx * dx :+ 0

render field bitmap = go 0
  where
    go i
      | i >= s * s = return ()
      | otherwise = do
        c <- read field i
        let (re :+ im) = 255 * (c + (1.5 :+ 1.5)) / 3
            r = floor re
            g = 0
            b = floor im
        write bitmap (i * 4) r
        write bitmap (i * 4 + 1) g
        write bitmap (i * 4 + 2) b
        go (i + 1)
