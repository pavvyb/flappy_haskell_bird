module Lib
    ( someFunc
    ) where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

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

initialPipe :: Height -> Pipe
initialPipe height = Pipe height 300

initialPipes :: StdGen -> [Pipe]
initialPipes stdGen = map initialPipe (randomRs pipesAllowedHeights stdGen)
-- initialPipes stdGen = map initialPipe (randomRs pipesAllowedHeights stdGen)
-- initialPipes = map (\v -> (Pipe height (v * 100 + 300))) [0,3..9]
--     where
--         height = -50

initialWorld :: StdGen -> World
initialWorld g = (initialGame, initialBird, initialPipes g)

main :: IO ()
main = do
    g <- newStdGen
    play windowDisplay rose 60 (initialWorld g) drawingFunc inputHandler updateFunc

drawingFunc :: World -> Picture
drawingFunc (Game Wait score bestScore, Bird height step, _) = bird height <> worldFloor
drawingFunc (Game Progress score bestScore, Bird height step, pipes) = drawScore score <> bird height <> worldFloor <> unionPicture (drawPipes pipes)
drawingFunc (Game EndGame score bestScore, Bird height step, pipes) =  bird height <> worldFloor <> unionPicture (drawPipes pipes) <> drawScoreBoard score bestScore

bird :: Height -> Picture
bird height = translate (-40) height (color yellow (circleSolid 20))

drawScore :: Int -> Picture
drawScore score = scale 0.5 0.5 (translate (-35) 80 (Text (show score)))

drawScoreBoard :: Score -> Score -> Picture
drawScoreBoard score bestScore = color orange (rectangleSolid 100 200) 
                                <>  translate (-8) (10) (scale 0.25 0.25 (Text (show score)))
                                <>  translate (-8) (-40) (scale 0.25 0.25 (Text (show bestScore)))
                                -- <> scale 0.25 0.25 (translate (-35) (-250) (Text (show bestScore)))
                                -- <> scale 0.25 0.25 (translate (-35) (-20) (Text (show score)))
                                -- <> scale 0.25 0.25 (translate (-35) (-250) (Text (show bestScore)))

worldFloor :: Picture
worldFloor = translate 0 (-250) $ color azure $ rectangleSolid 500 100

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
    | height > -200 + 20 = False
    | otherwise     = True

floor :: Height
floor = -200

pipesAllowedHeights :: (Height, Height)
pipesAllowedHeights = (-100, 100)

gapOnPipe :: Float
gapOnPipe = 150

gapBetweenPipes :: Float
gapBetweenPipes = 300

pipeWidth :: Float
pipeWidth = 50

pipesOnMap :: [Pipe] -> [Pipe]
pipesOnMap pipes = set 0 pipes
  where
    set _ []            = []
    set s ((Pipe h x) : pipes) = (Pipe h (x + s)) : set (s + x) pipes

drawPipes :: [Pipe] -> [Picture]
drawPipes = map drawPipe . takeWhile (\(Pipe _ x) -> x < 250) . pipesOnMap


drawPipe :: Pipe -> Picture
drawPipe (Pipe heightFromFloor horizontalPosition) = bottomPipe <> topPipe
    where
        bHeight = if heightFromFloor <= 0 then (200 - abs (heightFromFloor)) else 200 + heightFromFloor
        bY = -200 + bHeight / 2
        bottomPipe = translate horizontalPosition bY $ color green $ rectangleSolid pipeWidth bHeight

        tHeight = 1000
        tY = -200 + bHeight + 150 + tHeight / 2
        topPipe = translate horizontalPosition tY $ color green $ rectangleSolid pipeWidth tHeight

unionPicture :: [Picture] -> Picture
unionPicture []     = blank
unionPicture (x:xs)   = x <> unionPicture xs


updatePipes :: Float -> [Pipe] -> [Pipe]
updatePipes _ [] = []
updatePipes time ((Pipe height x) : gates)
  | currentX > (x - (-250) + 50)  = updatePipes (time - x / 100) gates
  | otherwise = (Pipe height (x - currentX)) : gates
  where
    currentX  = time * 100

updateScore :: [Pipe] -> Score -> Score
updateScore [] currentScore = currentScore
updateScore ((Pipe _ x):_) currentScore
    | x < -20 && x > -21  = currentScore + 1
    | otherwise = currentScore

collisionWithPipes :: [Pipe] -> Height -> Bool
collisionWithPipes pipes height = or (map (collisionWithPipe height) (takeWhile (\(Pipe _ x) -> x < 250) pipes))


collisionWithPipe :: Height -> Pipe -> Bool
collisionWithPipe birdHeight (Pipe h x) 
    | x <= -20 && x >= -60 && onBadHeight = True
    | otherwise             = False
        where
            onBadHeight = birdHeight <= (h + 20) || birdHeight >= (h + 150 - 20)

updateFunc :: Float -> World -> World
updateFunc time (Game mode score bestScore, Bird height step, pipes) = world
    where
        world
            | mode == Progress  = progressWorld
            | mode == EndGame   = endGameWorld
            | otherwise         = waitWorld
                where
                    newHeight = height - time * 25
                    newScore = updateScore pipes score
                    progressWorld
                        | stopFactor == False =                        (Game Progress newScore bestScore,
                                                                        Bird (height - time * 100) step,
                                                                        updatePipes time pipes)
                        | otherwise                                  = (Game EndGame newScore (if bestScore < newScore then newScore else bestScore),
                                                                        Bird (-200 + 20) step,
                                                                        updatePipes time pipes)
                    stopFactor :: Bool
                    stopFactor
                        | collisionWithPipes pipes newHeight || checkCollisionWithFloor newHeight = True
                        | otherwise = False


                    endGameWorld = (Game mode score bestScore, Bird height 0.4, pipes)
                    waitWorld    = (Game mode score bestScore, Bird (height + step) waitStep, tail (pipes))
                    waitStep
                        | height > 8 = -0.4
                        | height < 0 = 0.4
                        | otherwise  = step