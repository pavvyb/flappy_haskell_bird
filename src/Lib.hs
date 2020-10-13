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

-- main :: IO ()
-- main = animate windowDisplay rose animation
-- -- main = display FullScreen white (Circle 80)

-- animation :: Float -> Picture
-- animation time = placeAt (0, 10 * sin (time * 10)) (color yellow (circleSolid 20))
-- -- animation time = placeAt (0, 10 * sin (fromIntegral (floor(time * 1000) `mod` 90) ) / 10) (color yellow (circleSolid 20))
-- -- animation time = placeAt (0, wiggles 0 time) (color yellow (circleSolid 20))
-- -- animation time = placeAt (0, time) (color yellow (circleSolid 20)) 




-- type Position = (Float, Float)

-- placeAt :: Position -> Picture -> Picture
-- placeAt (x, y) picture  = translate x y picture

-- data Mode = Wait | ClassicGame | DynamicGame
data GameMode = Wait | Progress deriving (Eq)


-- wiggles :: Float -> Float -> Float
-- wiggles y time
--     | cos (10 * time) >= 1 = y - 20 * time
--     | otherwise = y + 20 * time


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
-- inputHandler _ (mode, height) = (Wait, height)
inputHandler _ w = w



updateFunc :: Float -> World -> World
updateFunc time (mode, height, step) = world
    where
        world
            | mode == Progress  = (mode, if (height - time * 25) > -100 then (height - time * 25) else -100, step)
            | otherwise         = (mode, height + step, if height > 8 then (-0.4) else if height < 0 then 0.4 else step) 
                where
                    -- ska :: Float
                    -- ska = 1
                    -- currentStep = step height 1
                    -- step :: Float -> Float -> Float
                    -- step h s
                    --     | h >= 100  && s == 1       = -s * 2
                    --     | h <= 0    && s == -1      = -s * 2
                    --     | otherwise                 = s

                    



-- updateFunc :: Float -> World -> World
-- updateFunc time (mode, height)
--     | val < 3             = (mode, height + 0.0001) 
--     | val >= 3 && val < 4 = (mode, height - 1) 
--     | val >= 4 && val < 7 = (mode, height + 1)                  
--     | otherwise            = (mode, height - 1) 
--         where
--         val = fromIntegral (floor(time) `mod` 8)
--                 -- direction = if height >= 100 then -1 else if height < 0 then 1 else 0 
--                 -- flooredTime = height + direction * 2 + 1tion = if height >= 100 then -1 else if height < 0 then 1 else 0 
--                 -- flooredTime = height + direction * 2 + 1
--                 -- flooredTime
--                 --     | height < 100     = height + 20
--                 --     | height >= 100    = height - 6
                
                    

-- updateFunc :: Float -> World -> World
-- updateFunc time (mode, height) = world
--     where
--         world
--             | mode == Progress  = (mode, if (height - time * 25) > -100 then (height - time * 25) else -100)
--             | otherwise         = (mode, flooredTime) 
--                 where
--                     flooredTime
--                         | height >= 100    = height - time * 25
--                         | height < 100     = height + time * 25
-- updateFunc :: Float -> World -> World
-- updateFunc time (mode, height) = world
--     where
--         world
--             | mode == Progress  = (mode, if (height - time * 25) > -100 then (height - time * 25) else -100)
--             | otherwise         = (mode, waitHeight) 
--                 where
--                     waitHeight 
--                         | sin (time * 10) > 1       = height - 10 * sin (time * 10)
--                         | otherwise                 = flooredTime
--                             where
--                                 flooredTime
--                                     | fromIntegral (floor(time) `mod` 10) < 5 = height + sin (time * 10)
--                                     | otherwise                 = height - sin (time * 10)
                    -- waitHeight = 10 * sin (time * 100)
            -- | otherwise         = (mode, height + time * 25) 
            -- | otherwise         = (mode, 10 * sin (time * 10)) 
                -- where
        -- towardCenter :: Float -> Float
        -- towardCenter c = if abs c < 0.25
        -- then 0
        -- else if c > 0
        --     then c - 0.25
        --     else c + 0.25
