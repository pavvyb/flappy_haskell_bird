module Lib
    ( someFunc
    ) where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

someFunc :: IO ()
someFunc = main

windowDisplay :: Display
windowDisplay = InWindow "Dynamic Flappy Bird" (800, 500) (10, 10)

type World = (Game, Bird)

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

initialWorld :: World
initialWorld = (initialGame, initialBird)

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
drawingFunc (Game Wait score bestScore, Bird height step) = translate 0 height (color yellow (circleSolid 20))
drawingFunc (Game Progress score bestScore, Bird height step) = drawScore score <> translate 0 height (color yellow (circleSolid 20))
drawingFunc (Game EndGame score bestScore, Bird height step) = drawScoreBoard score <> translate 0 height (color yellow (circleSolid 20))

drawScore :: Int -> Picture
drawScore score = scale 0.5 0.5 (translate (-35) 80 (Text (show score)))

drawScoreBoard :: Int -> Picture
drawScoreBoard score = color orange (rectangleSolid 100 200) <> scale 0.25 0.25 (translate (-35) (-20) (Text (show score)))

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) (Game mode score bestScore, Bird height step)
    | mode == Wait = (Game Progress 0 bestScore, Bird height 0.4)
    | otherwise    = (Game mode score bestScore, Bird height step)
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) world = jump world
inputHandler _ w = w

jump :: World -> World
jump (Game Progress score bestScore, Bird height step)      = (Game Progress score bestScore, Bird (height + 65) step)
jump (Game EndGame  score bestScore, bird)                  = (Game Wait     score bestScore, bird)
jump world = world

checkCollisionWithFloor :: Height -> Bool
checkCollisionWithFloor height
    | height > -100 = False
    | otherwise     = True


updateFunc :: Float -> World -> World
updateFunc time (Game mode score bestScore, Bird height step) = world
    where
        world
            | mode == Progress  = progressWorld
            | mode == EndGame   = endGameWorld
            | otherwise         = waitWorld
                where
                    newHeight = height - time * 25
                    progressWorld
                        | checkCollisionWithFloor newHeight == False = (Game Progress score bestScore, Bird (height - time * 100) step)
                        | otherwise                                  = (Game EndGame score bestScore,  Bird (-100) step)
                    
                    endGameWorld = (Game mode score bestScore, Bird height 0.4)
                    waitWorld    = (Game mode score bestScore, Bird (height + step) waitStep)
                    waitStep
                        | height > 8 = -0.4
                        | height < 0 = 0.4
                        | otherwise  = step