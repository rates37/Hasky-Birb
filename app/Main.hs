module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
    play
        window
        background
        fps
        initialState
        render
        handleInput
        update



-- Game constants:
windowWidth :: Int
windowWidth = 400

windowHeight :: Int
windowHeight = 600

gravity :: Float
gravity = -400

jumpVel :: Float
jumpVel = 200


-- Gloss Stuff:
window :: Display
window = InWindow "Flopy brid" (windowWidth, windowHeight) (10, 10)

background :: Color
background = makeColor 0.48 0.78 0.93 1.0

fps :: Int
fps = 60


-- Data Types:
data GameState = GameState {
    birdY :: Float,
    birdVel :: Float,
    gameTime :: Float
} deriving (Show)

data GameStatus = Playing | GameOver deriving (Eq, Show)

-- * Pipe (represented as x position and y gap position)


-- Define initial game state
initialState :: GameState 
initialState = GameState {
    birdY = 0,
    birdVel = 0,
    gameTime = 0
}


-- Update function(s)
update :: Float -> GameState -> GameState
-- state update method
update dt gs = gs {
    birdY = newBirdY,
    birdVel = newBirdVel,
    gameTime = (gameTime gs) + dt
}
    where
        newBirdVel = birdVel gs + gravity * dt
        newBirdY = birdY gs + newBirdVel * dt

-- Render function(s):
render :: GameState -> Picture
render gs = translate 0 (birdY gs) $ color red $ circleSolid 20



handleInput :: Event -> GameState -> GameState
-- handle whether in playing or gameover mode
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs = gs { birdVel = jumpVel }
handleInput _ gs = gs

