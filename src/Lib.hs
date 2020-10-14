module Lib
    ( someFunc
    ) where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

someFunc :: IO ()
someFunc = main

windowDisplay :: Display
windowDisplay = InWindow "Dynamic Flappy Bird" (500, 600) (10, 10)

type World = (Game, Bird, [Pipe])

data Bird = Bird 
    { height :: Height
    , step   :: Step}

type Height = Float
type Step = Float

data Game = Game
    { mode      :: GameMode 
    , score     :: Score
    , bestScore :: Score}

type Score = Int
data GameMode = Wait | Progress | EndGame deriving (Eq)

initialGame :: Game
initialGame = Game { mode = Wait, score = 0, bestScore = 0 }

initialBird :: Bird
initialBird = Bird { height = 0, step = 0.4 }

initialPipes :: [Pipe]
initialPipes = []

initialWorld :: World
initialWorld = (initialGame, initialBird, initialPipes)

main :: IO ()
main = play
    windowDisplay
    rose
    60
    initialWorld
    drawingFunc
    inputHandler
    updateFunc

drawingFunc :: World -> Picture
drawingFunc (Game Wait score bestScore, Bird height step, _) = bird height
drawingFunc (Game Progress score bestScore, Bird height step, _) = drawScore score <> bird height
drawingFunc (Game EndGame score bestScore, Bird height step, _) = drawScoreBoard score <> bird height

bird :: Height -> Picture
bird height = translate (-40) height (color yellow (circleSolid 20))

drawScore :: Int -> Picture
drawScore score = scale 0.5 0.5 (translate (-35) 80 (Text (show score)))

drawScoreBoard :: Int -> Picture
drawScoreBoard score = color orange (rectangleSolid 100 200) <> scale 0.25 0.25 (translate (-35) (-20) (Text (show score)))

-- Pipe
data Pipe = Pipe
    { heightFromFloor :: Height
    , horizontalPosition :: Float}


inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) (Game mode score bestScore, Bird height step, pipes)
    | mode == Wait = (Game Progress 0 bestScore, Bird height 0.4, pipes)
    | otherwise    = (Game mode score bestScore, Bird height step, pipes)
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) world = jump world
inputHandler _ w = w

jump :: World -> World
jump (Game Progress score bestScore, Bird height step, pipes)      = (Game Progress score bestScore, Bird (height + 65) step, pipes)
jump (Game EndGame  score bestScore, bird, pipes)                  = (Game Wait     score bestScore, bird, pipes)
jump world = world

checkCollisionWithFloor :: Height -> Bool
checkCollisionWithFloor height
    | height > -200 = False
    | otherwise     = True


floor :: Height
floor = -200

pipesAllowedHeights :: (Height, Height)
pipesAllowedHeights = (-100, 100)

gapOnPipe :: Float
gapOnPipe = 100

gapBetweenPipes :: Float
gapBetweenPipes = 300

pipeWidth :: Float
pipeWidth = 50


updateFunc :: Float -> World -> World
updateFunc time (Game mode score bestScore, Bird height step, pipes) = world
    where
        world
            | mode == Progress  = progressWorld
            | mode == EndGame   = endGameWorld
            | otherwise         = waitWorld
                where
                    newHeight = height - time * 25
                    progressWorld
                        | checkCollisionWithFloor newHeight == False = (Game Progress score bestScore,
                                                                        Bird (height - time * 100) step,
                                                                        pipes)
                        | otherwise                                  = (Game EndGame score bestScore,
                                                                        Bird (-200) step,
                                                                        pipes)
                    
                    endGameWorld = (Game mode score bestScore, Bird height 0.4, pipes)
                    waitWorld    = (Game mode score bestScore, Bird (height + step) waitStep, pipes)
                    waitStep
                        | height > 8 = -0.4
                        | height < 0 = 0.4
                        | otherwise  = step