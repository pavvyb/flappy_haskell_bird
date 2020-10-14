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
-- type World = (GameMode, Height, Step, Score)

-- data Bird = Bird Height Step          -- height step
data Bird = Bird 
    { height :: Height
    , step   :: Step}

type Height = Float
type Step = Float

-- data Game = Game GameMode Score Score -- mode score bestScore
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
    -- ((Wait, 0, 0), )
    -- (Wait, 0, 0.4, 0)
    drawingFunc
    inputHandler
    updateFunc

drawingFunc :: World -> Picture
-- drawingFunc (Wait, height, _, _) = translate 0 height (color yellow (circleSolid 20))
-- drawingFunc (Progress, height, _, score) = drawScore score <> translate 0 height (color yellow (circleSolid 20))
-- drawingFunc (EndGame, height, _, score) = drawScoreBoard score <> translate 0 height (color yellow (circleSolid 20))
drawingFunc (Game Wait score bestScore, Bird height step) = translate 0 height (color yellow (circleSolid 20))
drawingFunc (Game Progress score bestScore, Bird height step) = drawScore score <> translate 0 height (color yellow (circleSolid 20))
drawingFunc (Game EndGame score bestScore, Bird height step) = drawScoreBoard score <> translate 0 height (color yellow (circleSolid 20))

drawScore :: Int -> Picture
drawScore score = scale 0.5 0.5 (translate (-35) 80 (Text (show score)))

drawScoreBoard :: Int -> Picture
drawScoreBoard score = color orange (rectangleSolid 100 200) <> scale 0.25 0.25 (translate (-35) (-20) (Text (show score)))

inputHandler :: Event -> World -> World
-- inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) (_, height, _, score) = (Progress, height, 0.4, score)
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) (Game mode score bestScore, Bird height step)
    | mode == Wait = (Game Progress 0 bestScore, Bird height 0.4)
    | otherwise    = (Game mode score bestScore, Bird height step)
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) world = jump world
inputHandler _ w = w

-- jump :: World -> World
-- jump (Wait, height, step, score) = (Wait, height, step, score)
-- jump (Progress, height, step, score) = (Progress, height + 65, step, score)
-- jump (EndGame, height, step, score) = (Wait, height, step, score)

jump :: World -> World
jump (Game Progress score bestScore, Bird height step)      = (Game Progress score bestScore, Bird (height + 65) step)
jump (Game EndGame  score bestScore, bird)                  = (Game Wait     score bestScore, bird)
jump world = world

-- updateFunc :: Float -> World -> World
-- updateFunc time (mode, height, step, score) = world
--     where
--         world
--             | mode == Progress  = (if (height - time * 25) > -100 then Progress else EndGame, if (height - time * 25) > -100 then (height - time * 100) else -100, step, score)
--             | mode == EndGame   = (mode, height, 0.4, score) 
--             | otherwise         = (mode, height + step, if height > 8 then (-0.4) else if height < 0 then 0.4 else step, score)

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
                    -- waitStep     = if height > 8 then (-0.4) else if height < 0 then 0.4 else step
                    waitStep
                        | height > 8 = -0.4
                        | height < 0 = 0.4
                        | otherwise  = step

-- updateFunc :: Float -> World -> World
-- updateFunc time (Game mode score bestScore, Bird height step) = world
--     where
--         world
--             | mode == Progress  = (if (height - time * 25) > -100 then Progress else EndGame, if (height - time * 25) > -100 then (height - time * 100) else -100, step, score)
--             | mode == EndGame   = (mode, height, 0.4, score) 
--             | otherwise         = (mode, height + step, if height > 8 then (-0.4) else if height < 0 then 0.4 else step, score)
--                 where
--                     newHeight = height - time * 25
--                     progressWorld
--                         | checkCollisionWithFloor newHeight == False = (Game Progress score bestScore, Bird (height - time * 100) step)
--                         | otherwise                                  = (Game EndGame score bestScore,  Bird (-100) step)
                    
--                     endGameWorld = (Game mode score bestScore, Bird height 0.4)
--                     waitWorld    = (Game mode, score, bestScore, Bird (height + step) waitStep)
--                     waitStep     = if height > 8 then (-0.4) else if height < 0 then 0.4 else step