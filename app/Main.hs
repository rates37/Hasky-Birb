{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Fixed (mod')
import Data.List (mapAccumL)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  play
    window
    background
    fps
    (initialState gen)
    render
    handleInput
    update

-- ! Game constants:
windowWidth :: Int
windowWidth = 400

windowHeight :: Int
windowHeight = 600

gravity :: Float
gravity = -850

jumpVel :: Float
jumpVel = 350

initialScrollSpeed :: Float
initialScrollSpeed = 200

speedUpFactor :: Float
speedUpFactor = 3

-- Gloss Stuff:
window :: Display
window = InWindow "Hasky Birb" (windowWidth, windowHeight) (10, 10)

background :: Color
background = makeColor 0.48 0.78 0.93 1.0

fps :: Int
fps = 60

pipeSpacing :: Float
pipeSpacing = 260.0

pipeWidth :: Float
pipeWidth = 70.0

pipeGap :: Float
pipeGap = 160.0

birdX :: Float
birdX = -60.0

birdRadius :: Float
birdRadius = 20.0

-- ! Data Types:
data GameState = GameState
  { birdY :: Float,
    birdVel :: Float,
    gameTime :: Float,
    gameStatus :: GameStatus,
    pipes :: [Pipe],
    rng :: StdGen,
    score :: Int
  }
  deriving (Show)

data GameStatus = Playing | GameOver | Menu deriving (Eq, Show)

type Pipe = (Float, Float, Bool) -- represented as x position and y gap position

-- Define initial game state
initialState :: StdGen -> GameState
initialState g =
  GameState
    { birdY = 0,
      birdVel = 0,
      gameTime = 0,
      gameStatus = Menu,
      pipes = initialPipes g,
      rng = g,
      score = 0
    }

-- ! Update function(s)
update :: Float -> GameState -> GameState
update dt gs@GameState {..} =
  case gameStatus of
    Playing -> updatePlaying dt gs
    _ -> gs {gameTime = gameTime + dt} -- Only update time for cloud animation

updatePlaying :: Float -> GameState -> GameState
updatePlaying dt gs@GameState {..} =
  let currentScrollSpeed = initialScrollSpeed + fromIntegral score * speedUpFactor
      newVel = birdVel + gravity * dt
      newY = birdY + newVel * dt

      -- Check collision before updating pipes
      collidedEarly = checkCollision gs {birdY = newY}

      movedPipes = map (\(x, y, p) -> (x - currentScrollSpeed * dt, y, p)) pipes
      (scoredPipes, newScore) = updateScore gs {pipes = movedPipes}
      filteredPipes = filter (\(x, _, _) -> x > -fromIntegral windowWidth / 2 - pipeWidth) scoredPipes
      (finalPipes, newGen) = addNewPipes rng filteredPipes currentScrollSpeed

      collided = collidedEarly || checkCollision gs {birdY = newY, pipes = finalPipes}
      newStatus = if collided then GameOver else Playing
   in gs
        { birdY = newY,
          birdVel = newVel,
          pipes = finalPipes,
          rng = newGen,
          score = newScore,
          gameStatus = newStatus,
          gameTime = gameTime + dt
        }

updateScore :: GameState -> ([Pipe], Int)
updateScore GameState {..} = (newPipes, newScore)
  where
    (newPipes, scoreToAdd) = foldr f ([], 0) pipes
    newScore = score + scoreToAdd
    f pipe@(px, py, passed) (accPipes, accScore)
      | not passed && px < birdX = ((px, py, True) : accPipes, accScore + 1)
      | otherwise = (pipe : accPipes, accScore)

addNewPipes :: StdGen -> [Pipe] -> Float -> ([Pipe], StdGen)
addNewPipes gen ps currentSpeed =
  if shouldAddPipe then (ps ++ [newPipe], newGen) else (ps, gen)
  where
    lastPipeX = case ps of
      [] -> 0
      _ -> let (x, _, _) = last ps in x
    shouldAddPipe = lastPipeX < (fromIntegral windowWidth / 2 - pipeSpacing + currentSpeed * dt)
    (newGapY, newGen) = randomR (-160.0, 160.0) gen
    newPipe = (lastPipeX + pipeSpacing, newGapY, False)
    dt = 1 / 60

checkCollision :: GameState -> Bool
checkCollision GameState {..} = groundCollision || ceilingCollision || pipeCollision
  where
    halfWindowH = fromIntegral windowHeight / 2
    birdTop = birdY + birdRadius
    birdBottom = birdY - birdRadius
    groundCollision = birdBottom < -halfWindowH
    ceilingCollision = birdTop > halfWindowH
    pipeCollision = any (isCollidingWithPipe birdY) pipes

isCollidingWithPipe :: Float -> Pipe -> Bool
isCollidingWithPipe y (px, gapY, _) =
  let birdRad = birdRadius
      pipeHalfGap = pipeGap / 2
      pipeTop = gapY + pipeHalfGap
      pipeBottom = gapY - pipeHalfGap
   in (birdX + birdRad > px - pipeWidth / 2 && birdX - birdRad < px + pipeWidth / 2)
        && (y + birdRad > pipeTop || y - birdRad < pipeBottom)

-- ! Render function(s):
render :: GameState -> Picture
render gs@GameState {..} =
  pictures
    [ drawClouds gameTime,
      case gameStatus of
        Playing -> renderGame gs
        Menu -> renderMenu
        GameOver -> pictures [renderGame gs, renderGameOver score]
    ]

renderGame :: GameState -> Picture
renderGame GameState {..} =
  pictures
    [ pictures (map drawPipe pipes),
      translate 0 birdY $ drawBird birdVel gameTime,
      drawTextWithShadow (show score) (0, fromIntegral windowHeight / 2 - 80) 0.4
    ]

renderMenu :: Picture
renderMenu =
  pictures
    [ drawTextWithShadow "Hasky birb" (-100, 50) 0.3,
      drawTextWithShadow "Press space to begin" (-100, -20) 0.15
    ]

renderGameOver :: Int -> Picture
renderGameOver finalScore =
  pictures
    [ -- Put dark tint on game:
      color (makeColor 0 0 0 0.5) $ rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight),
      -- Show score:
      drawTextWithShadow "Game Over" (-100, 50) 0.3,
      drawTextWithShadow ("Score: " ++ (show finalScore)) (-80, 0) 0.2,
      drawTextWithShadow "Press 'R' to restart" (-110, -50) 0.2
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

drawBird :: Float -> Float -> Picture
drawBird v t =
  rotate birdRotation $
    pictures
      [ color (makeColor 1.0 0.84 0.0 1.0) $ circleSolid 20, -- body
        translate 7 5 $ color white $ circleSolid 5, -- eye ball
        translate 7 5 $ color black $ circleSolid 2, -- eye pupil
        translate 22 (0 - 5) $ color red $ circleSolid 5, -- beak
        translate (-5) wingY $ rotate (-20) $ color (makeColor 0.9 0.7 0 1) $ rectangleSolid 10 15 -- wing
      ]
  where
    birdRotation = clamp (-45) 30 (v / 4)
    wingY = 10 * sin (t * 20)

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
handleGameOverInput (EventKey (Char 'r') Down _ _) gs =
  let (newGen, _) = splitGen (rng gs)
   in initialState newGen
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

{-
def initialPipes() -> list[Pipe]:
    initialXPositions = [250 + (i*pipeSpacing)+250 for i in range(3)]

    return [
    (x, random.choice(lo=-160, hi=160), False) for x in initialXpositions]
-}
initialPipes :: StdGen -> [Pipe]
initialPipes g = snd $ mapAccumL generateNextPipe g [250, 250 + pipeSpacing, 250 + 2 * pipeSpacing]
  where
    generateNextPipe gen x =
      let (gapY, newGen) = randomR (-160, 160) gen
       in (newGen, (x, gapY, False))

{-
def drawPipe(pipe: Pipe) -> Picture:
    x,y,_ = pipe

    pipeColor = makeCOlor(0.4, 0.8, 0.4, 1)
    pipeBody = color(pipeColor, rectangleSolid (pipeWidth, float(windowHeight)))

    topPipeY = ...
    topPipeX = ...

    topPipePic = translate(x, topPipeY, pipeBody)
    bottomPipePic = translate(x, bottomPipeY, pipeBody)

    return pictures([topPipePic, bottomPipePic])

-}
drawPipe :: Pipe -> Picture
drawPipe (px, gy, _) =
  pictures
    [ translate px (gy + pipeGap / 2 + fromIntegral windowHeight / 2) pipeBody,
      translate px (gy - pipeGap / 2 - fromIntegral windowHeight / 2) pipeBody
    ]
  where
    pipeColour = makeColor 0.4 0.8 0.4 1.0
    pipeBody = color pipeColour $ rectangleSolid pipeWidth (fromIntegral windowHeight)
