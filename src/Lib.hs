module Lib
    ( someFunc
    ) where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
-- import Graphics.Image.Processing
import System.IO.Unsafe (unsafePerformIO)
import System.Random

someFunc :: IO ()
someFunc = main

windowDisplay :: Display
windowDisplay = InWindow "Dynamic Flappy Bird" (500, 600) (10, 10)

data Background = Background
    { pic   :: Picture
    , bStep :: Step }

-- | Type respresents the World of application.
type World = (Game, Bird, [Pipe], Step, Step)

-- | Flappy (and not only) bird. 
data Bird = Bird 
    { height :: Height
    , step   :: Step}

-- | Height type used in the world. Represents the height of bird and pipes.
type Height = Float

-- | Step type is the speed of Bird (falling up and down).
type Step = Float

-- | Game data. Responsible for game parameters such as game mode, score result
-- best score result and id of chosen bird.
data Game = Game
    { mode      :: GameMode 
    , score     :: Score
    , bestScore :: Score
    , bIndex    :: Int }

-- | Score type used for user's score representation in Game.
type Score = Int

-- | Mode of the Game. Game could be in one possible mode:
-- Wait - when user could choose a bird
-- Progress - game itself
-- EndGame - when user can observe game results.
data GameMode = Wait | Progress | EndGame deriving (Eq)

-- | Initial game state. Starts from Wait mode, zero score and bird with id=0
initialGame :: Game
initialGame = Game { mode = Wait, score = 0, bestScore = 0, bIndex = 0 }

-- | Initial bird state. Starts from height 0 and step 0.4 (for smooth up and down while waiting)
initialBird :: Bird
initialBird = Bird { height = 0, step = 0.4 }

-- | Initial pipe with its X shift
initialPipe :: Height -> Pipe
initialPipe height = Pipe height 300

-- | Initializes endless pipes in the game.
initialPipes :: StdGen -> [Pipe]
initialPipes stdGen = map initialPipe (randomRs pipesAllowedHeights stdGen)

-- | Initial world state.
initialWorld :: StdGen -> World
initialWorld g = (initialGame, initialBird, initialPipes g, 0, 0)

main :: IO ()
main = do
    g <- newStdGen
    play windowDisplay rose 60 (initialWorld g) drawingFunc inputHandler updateFunc

returnMovingBgd :: Float -> Picture
returnMovingBgd seconds = translate (fromIntegral ((floor (seconds/20)) `mod` 500) - 350) 0 compositeImage
                            where
                                compositeImage = translate (-350) 0 (loadPicture "pictures/background.bmp")
                                               <> translate (150) 0 (loadPicture "pictures/background.bmp")
                                               <> translate (650) 0 (loadPicture "pictures/background.bmp")
                                            --    <> translate (1150) 0 (loadPicture "pictures/background.bmp")
                                            --    <> translate (1650) 0 (loadPicture "pictures/background.bmp")


returnMovingFloor :: Float -> Picture
-- returnMovingFloor seconds = translate (fromIntegral ((floor (seconds/5)) `mod` 1000) - 350) 0 compositeImage
returnMovingFloor seconds = translate (fromIntegral ((floor (seconds)) `mod` 1000) - 350) 0 compositeImage
                            where
                              compositeImage = translate (-400) (-250) (loadPicture "pictures/floor.bmp")
                                               <> translate 600 (-250) (loadPicture "pictures/floor.bmp")
                                               <> translate 1600 (-250) (loadPicture "pictures/floor.bmp")

-- | Responsible for drawing each possible state in the game (in the modes: Wait, Progress, EndGame)
drawingFunc :: World -> Picture
drawingFunc (Game Wait _ _ bIndex, Bird height _, _, backgroundStep, floorStep) = returnMovingBgd backgroundStep --(loadPicture "pictures/background.bmp")
                                                                      <> drawArrows
                                                                      <> worldFloor 
                                                                      <> chooseBird bIndex height
drawingFunc (Game Progress score _ bIndex, Bird height time, pipes, backgroundStep, floorStep)    = returnMovingBgd backgroundStep
                                                                      <> returnMovingFloor floorStep 
                                                                      <> unionPicture (drawPipes pipes)
                                                                      <> chooseBird bIndex height 
                                                                      <> drawScore score
drawingFunc (Game EndGame score bestScore bIndex, Bird height _, pipes, backgroundStep, floorStep) = returnMovingBgd backgroundStep -- (loadPicture "pictures/background.bmp")
                                                                          <> worldFloor
                                                                          <> unionPicture (drawPipes pipes) 
                                                                          <> chooseBird bIndex height 
                                                                          <> drawScoreBoard score bestScore

-- | Loads picture from path (represented as a string)
loadPicture :: String -> Picture
loadPicture path = unsafePerformIO $ loadBMP path

-- | All possible birds user could choose in the game.
birds :: Height -> [Picture]
birds height = birdsPictures
    where
        birdsPictures = [ translate (-40) height (color red (circleSolid 20))
                        , translate (-40) height (color yellow (polygon [(-20, -20), (0, 20), (20, -20)]))
                        , translate (-40) height (color black (rectangleSolid 40 40))
                        , translate (-40) height (loadPicture "pictures/blue.bmp")
                        , translate (-40) height (loadPicture "pictures/red.bmp")
                        , translate (-40) height (loadPicture "pictures/yellow.bmp")
                        , translate (-40) height (loadPicture "pictures/haha.bmp")
                        , translate (-40) height (loadPicture "pictures/lit.bmp")]
 
-- | Chosen by user bird for playing.
chooseBird :: Int -> Height -> Picture
chooseBird index height = (birds height)!!index

-- | Draws score in Progress game mode.
drawScore :: Int -> Picture
drawScore score = scale 0.5 0.5 (translate (-35) 220 (Text (show score)))

-- | Draws score board in EndGame game mode.
drawScoreBoard :: Score -> Score -> Picture
drawScoreBoard score bestScore = color orange (rectangleSolid 100 200) 
                                <>  translate (-8) (10) (scale 0.25 0.25 (Text (show score)))
                                <>  translate (-8) (-40) (scale 0.25 0.25 (Text (show bestScore)))

-- | Draws arrows to switch between birds.
drawArrows :: Picture
drawArrows = leftArrow <> rightArrow
             where
               preLeft = polygon [(-15, 0), (0, 20), (0, -20)]
               preRight = polygon [(15, 0), (0, 20), (0, -20)]

               colLeft = color white preLeft
               colRight = color white preRight

               leftArrow = translate (-100) 0 colLeft
               rightArrow = translate 20 0 colRight

-- | Returns a picture of the game's floor.
worldFloor :: Picture
worldFloor = translate 0 (-250) $ loadPicture "pictures/floor.bmp"

-- | Pipe data. Each pipe contain height from floor and horizontal position (shift).
data Pipe = Pipe
    { heightFromFloor :: Height
    , horizontalPosition :: Float}

-- | Input handler responsible for handling the keys to control the game.
inputHandler :: Event -> World -> World
-- | By KeySpace there is jump action done (depends on mode)
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) world = jump world
-- | By KeyRight the switching to the right (between birds) is done (if possible)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (Game Wait score bestScore bIndex, Bird height x, pipes, backgroundStep, floorStep)
    | bIndex < 0 || bIndex >= (length (birds height) - 1) = (Game Wait score bestScore bIndex, Bird height x, pipes, backgroundStep, floorStep)
    | otherwise = (Game Wait score bestScore (bIndex + 1), Bird height x, pipes, backgroundStep, floorStep)
-- | By KeyLeft the switching to the right (between birds) is done (if possible)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (Game Wait score bestScore bIndex, Bird height x, pipes, backgroundStep, floorStep)
    | bIndex <= 0 || bIndex > (length (birds height) - 1) = (Game Wait score bestScore bIndex, Bird height x, pipes, backgroundStep, floorStep)
    | otherwise = (Game Wait score bestScore( bIndex - 1), Bird height x, pipes, backgroundStep, floorStep) 
-- | Otherwise return same world was come as input.
inputHandler _ w = w

-- | Jump function for Bird. Depends on modes:
-- On Wait mode starts the game with initial step == 350
-- On Progress mode bird jumps if possible.
-- On EndGame mode switches to Wait mode
jump :: World -> World
jump (Game Wait _ bestScore bIndex, Bird height _, pipes, backgroundStep, floorStep)                 = (Game Progress 0 bestScore bIndex, Bird height 350, pipes, backgroundStep, floorStep)
jump (Game Progress score bestScore bIndex, Bird height step, pipes, backgroundStep, floorStep)      = (Game Progress score bestScore bIndex, Bird height (if step < 0 then 350 else step), pipes, backgroundStep, floorStep)
jump (Game EndGame  score bestScore bIndex, bird, pipes, backgroundStep, floorStep)                  = (Game Wait     score bestScore bIndex, bird, pipes, backgroundStep, floorStep)

-- | Responsible for checking collision with floor.
-- Collision occures if the lowest part of bird is lower than (-200 + radius of bird)
collisionWithFloor :: Height -> Bool
collisionWithFloor height
    | height > -200 + 20 = False
    | otherwise     = True

-- | Range of pipes allowed heights.
pipesAllowedHeights :: (Height, Height)
pipesAllowedHeights = (-100, 100)

-- | Width of single pipe.
pipeWidth :: Float
pipeWidth = 75

-- | "Places" pipes on the map.
pipesOnMap :: [Pipe] -> [Pipe]
pipesOnMap pipes = set 0 pipes
  where
    set _ []            = []
    set s ((Pipe h x) : pipes) = (Pipe h (x + s)) : set (s + x) pipes

-- | Draws only pipe which are shown on window.
drawPipes :: [Pipe] -> [Picture]
drawPipes = map drawPipe . takeWhile (\(Pipe _ x) -> x < 250) . pipesOnMap

-- | Draws single pipe (its bottom and top parts using very complex mathematical computations.)
drawPipe :: Pipe -> Picture
drawPipe (Pipe heightFromFloor horizontalPosition) = bottomPipe <> topPipe
    where
        bHeight = if heightFromFloor <= 0 then (200 - abs (heightFromFloor)) else 200 + heightFromFloor
        bY = -200 + bHeight / 2
        -- bottomPipe = translate horizontalPosition bY $ resize  Bilinear Edge (75, bHeight) (loadPicture "pictures/pipe.bmp")
        bottomPipe = translate horizontalPosition bY $ color green $ rectangleSolid pipeWidth bHeight

        tHeight = 1000
        tY = -200 + bHeight + 150 + tHeight / 2
        -- topPipe = translate horizontalPosition tY $ resize Bilinear Edge (75, tHeight) (loadPicture "pictures/pipe.bmp")
        topPipe = translate horizontalPosition tY $ color green $ rectangleSolid pipeWidth tHeight

-- | Unions list of pictures to a single picture.
unionPicture :: [Picture] -> Picture
unionPicture []         = blank
unionPicture (x:xs)     = x <> unionPicture xs

-- | Update pipes on World.
updatePipes :: Float -> [Pipe] -> [Pipe]
updatePipes _ [] = []
updatePipes time ((Pipe height x) : pipes)
  | currentX > (x - (-250) + pipeWidth)  = updatePipes (time - x / 150) pipes
  | otherwise = (Pipe height (x - currentX)) : pipes
  where
    currentX  = time * 150

-- | Update score on Game by checking was pipe passed or not.
updateScore :: [Pipe] -> Score -> Float -> Score
updateScore [] currentScore _ = currentScore
updateScore pipes currentScore time
    | passed = currentScore + 1
    | otherwise  = currentScore
        where
            passed :: Bool
            passed = not (null (takeWhile now (dropWhile was (pipesOnMap pipes))))
            now :: Pipe -> Bool
            now (Pipe _ x) = x - time * 150 < -20
            was :: Pipe -> Bool
            was (Pipe _ x)= x < -20

-- | Responsible for checking collisions of bird with that are displayed on window screen.
collisionWithPipes :: [Pipe] -> Height -> Bool
collisionWithPipes pipes height = or (map (collisionWithPipe height) (takeWhile (\(Pipe _ x) -> x < 250) (pipesOnMap pipes)))

-- | Responsible for checking collisions of bird with single pipe (again using very complex mathematical computations.)
collisionWithPipe :: Height -> Pipe -> Bool
collisionWithPipe birdHeight (Pipe h x) 
    | onBadX && onBadHeight = True
    | otherwise             = False
        where
            onBadHeight = birdHeight <= (h + 20) || birdHeight >= (h + 150 - 20)
            onBadX
                | leftPipeX <= badMaxX && rightPipeX >= badMinX = True
                | otherwise                     = False
                    where
                        badMinX = -60
                        badMaxX = -20
                        leftPipeX = x - pipeWidth / 2
                        rightPipeX = x + pipeWidth / 2

-- | Responsible for updating the world.
updateFunc :: Float -> World -> World
updateFunc time (Game mode score bestScore bIndex, Bird height step, pipes, backgroundStep, floorStep) = world
    where
        world
            | mode == Progress  = progressWorld -- Show the Progress mode state
            | mode == EndGame   = endGameWorld  -- Show the EndGame mode state
            | otherwise         = waitWorld     -- Show the Wait mode state
                where
                    newHeight = height + time * newStep -- calculate new height
                    newStep = step - time * 1000 -- calculate the step
                    newScore = updateScore pipes score time -- calculate new Score
                    progressWorld
                        | stopFactor == False =                        (Game Progress newScore bestScore bIndex,
                                                                        Bird newHeight newStep,
                                                                        updatePipes time pipes, backgroundStep - time * 1000, floorStep - time * 150)
                        | otherwise                                  = (Game EndGame newScore (if bestScore < newScore then newScore else bestScore) bIndex,
                                                                        Bird (-200 + 20) step,
                                                                        updatePipes time pipes, backgroundStep - time * 1000, floorStep - time * 150)
                    -- | Stop factor function that is responsible for detecting collisions.
                    stopFactor :: Bool
                    stopFactor
                        | collisionWithPipes pipes newHeight || collisionWithFloor newHeight = True
                        | otherwise = False
                    endGameWorld = (Game mode score bestScore bIndex, Bird height 0.4, pipes, backgroundStep, floorStep)

                    -- | Wait game mode smoothing up and down falling.
                    waitWorld    = (Game mode score bestScore bIndex, Bird (boundedHeight height + step) waitStep, tail (pipes), backgroundStep, floorStep)
                    boundedHeight h = if h < -8 then 0 else h
                    waitStep
                        | height > 8 = -0.4
                        | height < 0 = 0.4
                        | otherwise  = step