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

data GameMode = Wait | Progress deriving (Eq)

type World = (GameMode, Height, Step)
type Height = Float
type Step = Float

main :: IO ()
main = play
    windowDisplay
    rose
    60
    (Wait, 0, 0.4)
    drawingFunc
    inputHandler
    updateFunc

drawingFunc :: World -> Picture
drawingFunc (_, height, _) = translate 0 height (color yellow (circleSolid 20))

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) (_, height, _) = (Progress, height, 0.4)
-- inputHandler (EventKey (SpecialKey KeySpace) Down _ _) (mode, height) = (x + 10, y)
inputHandler _ w = w


updateFunc :: Float -> World -> World
updateFunc time (mode, height, step) = world
    where
        world
            | mode == Progress  = (mode, if (height - time * 25) > -100 then (height - time * 25) else -100, step)
            | otherwise         = (mode, height + step, if height > 8 then (-0.4) else if height < 0 then 0.4 else step) 