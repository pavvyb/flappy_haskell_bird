module Lib
    ( someFunc
    ) where


import Graphics.Gloss
-- import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

someFunc :: IO ()
someFunc = main

windowDisplay :: Display
windowDisplay = InWindow "Dynamic Flappy Bird" (800, 500) (10, 10)

data GameMode = Wait | Progress | EndGame deriving (Eq)

type World = (GameMode, Height, Step, Score)
type Height = Float
type Step = Float
type Score = Int

main :: IO ()
main = play
    windowDisplay
    rose
    60
    (Wait, 0, 0.4, 0)
    drawingFunc
    inputHandler
    updateFunc

drawingFunc :: World -> Picture
drawingFunc (Wait, height, _, _) = translate 0 height (color yellow (circleSolid 20))
drawingFunc (Progress, height, _, score) = drawScore score <> translate 0 height (color yellow (circleSolid 20))
drawingFunc (EndGame, height, _, score) = drawScoreBoard score <> translate 0 height (color yellow (circleSolid 20))


drawScore :: Int -> Picture
drawScore score = scale 0.5 0.5 (translate (-35) 80 (Text (show score)))

drawScoreBoard :: Int -> Picture
drawScoreBoard score = color orange (rectangleSolid 100 200) <> scale 0.25 0.25 (translate (-35) (-20) (Text (show score)))

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) (_, height, _, score) = (Progress, height, 0.4, score)
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) world = jump world
inputHandler _ w = w

jump :: World -> World
jump (Wait, height, step, score) = (Wait, height, step, score)
jump (Progress, height, step, score) = (Progress, height + 65, step, score)
jump (EndGame, height, step, score) = (Wait, height, step, score)

updateFunc :: Float -> World -> World
updateFunc time (mode, height, step, score) = world
    where
        world
            | mode == Progress  = (if (height - time * 25) > -100 then Progress else EndGame, if (height - time * 25) > -100 then (height - time * 100) else -100, step, score)
            | mode == EndGame   = (mode, height, 0.4, score) 
            | otherwise         = (mode, height + step, if height > 8 then (-0.4) else if height < 0 then 0.4 else step, score) 