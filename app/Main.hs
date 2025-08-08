{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Fixed (mod')
import Data.List (mapAccumL)
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

-- ! Game constants:
windowWidth :: Int
windowWidth = 400

windowHeight :: Int
windowHeight = 600

gravity :: Float
gravity = -550

jumpVel :: Float
jumpVel = 200

-- Gloss Stuff:
window :: Display
window = InWindow "Hasky Birb" (windowWidth, windowHeight) (10, 10)

background :: Color
background = makeColor 0.48 0.78 0.93 1.0

fps :: Int
fps = 60

-- ! Data Types:
data GameState = GameState
  { birdY :: Float,
    birdVel :: Float,
    gameTime :: Float,
    gameStatus :: GameStatus
  }
  deriving (Show)

data GameStatus = Playing | GameOver | Menu deriving (Eq, Show)

-- * Pipe (represented as x position and y gap position)

-- Define initial game state
initialState :: GameState
initialState =
  GameState
    { birdY = 0,
      birdVel = 0,
      gameTime = 0,
      gameStatus = Menu
    }

-- ! Update function(s)
update :: Float -> GameState -> GameState
-- state update method
update dt gs =
  gs
    { birdY = newBirdY,
      birdVel = newBirdVel,
      gameTime = (gameTime gs) + dt
    }
  where
    newBirdVel = birdVel gs + gravity * dt
    newBirdY = birdY gs + newBirdVel * dt

-- ! Render function(s):
render :: GameState -> Picture
render gs@GameState {..} =
  pictures
    [ drawClouds gameTime,
      case gameStatus of
        Playing -> renderGame gs
        _ -> renderMenu
    ]

renderGame :: GameState -> Picture
renderGame gs@GameState {..} =
  pictures
    [ drawClouds gameTime,
      translate 0 birdY $ color red $ circleSolid 20
    ]

renderMenu :: Picture
renderMenu =
  pictures
    [ drawTextWithShadow "Hasky birb" (-100, 50) 0.3,
      drawTextWithShadow "Press space to begin" (-100, -20) 0.15
    ]

{-
Equivalent non haskelly code:
def drawClouds(time: float) -> [Picture]:
    clouds = [(1.0, 200, 150, 40), ... ]

    def drawCloud(cloudData: tuple[speed: float, x: float, y: float, r: float]) -> picture:
        # makes a single cloud
        w = float(windowWidth)
        xPos = x - ((time * 40 * speed) % (w+r*2))
        return translate(xPos, y, color(makeColor(1,1,1,1), circleSolid(r)))

    return pictures(list(map(drawCloud, clouds)))
-}
drawClouds :: Float -> Picture
drawClouds t = pictures $ map drawCloud clouds
  where
    clouds =
      [ -- Speed, x, y, radius
        (1, 200, 150, 40),
        (0.7, -50, 50, 60),
        (0.8, 300, -100, 50),
        (0.9, 100, -200, 35)
      ]
    drawCloud (speed, x, y, r) =
      let w = fromIntegral windowWidth
          xPos = x - (mod' (t * 40 * speed) (w + r * 2))
       in translate xPos y $ color (makeColor 1 1 1 1) $ circleSolid r


-- ! Event handling:
handleInput :: Event -> GameState -> GameState
-- handle whether in playing or gameover mode
-- handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs = gs {birdVel = jumpVel}
handleInput event gs@GameState {..} = 
    case gameStatus of
        Menu -> handleMenuInput event gs
        Playing -> handlePlayingInput event gs
        GameOver -> handleGameOverInput event gs

handleMenuInput :: Event -> GameState -> GameState
handleMenuInput (EventKey (SpecialKey KeySpace) Down _ _) gs = gs {gameStatus = Playing, birdVel = jumpVel}
handleMenuInput _ gs = gs

handlePlayingInput :: Event -> GameState -> GameState
handlePlayingInput (EventKey (SpecialKey KeySpace) Down _ _) gs = gs {birdVel = jumpVel}
handlePlayingInput _ gs = gs

handleGameOverInput :: Event -> GameState -> GameState
handleGameOverInput (EventKey (Char 'r') Down _ _) gs@GameState {..} = initialState
handleGameOverInput _ gs = gs

-- Misc functions:
clamp :: (Ord a) => a -> a -> a -> a
clamp lo hi val = max lo (min val hi)

drawTextWithShadow :: String -> (Float, Float) -> Float -> Picture
drawTextWithShadow str (x, y) s =
  pictures
    [ translate (x + 2) (y - 2) $ scale s s $ color (makeColor 0 0 0 0.5) $ text str, -- this is the shadow
      translate x y $ scale s s $ color white $ text str -- White text
    ]
