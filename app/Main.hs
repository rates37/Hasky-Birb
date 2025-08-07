module Main (main) where

import Graphics.Gloss

window :: Display
window = InWindow "Flopy brid" (420, 420) (100, 100)

background :: Color
background = white

drawing :: Picture
drawing = Pictures [thickCircle 80 2, circle 50]

main :: IO ()
main = display window background drawing
