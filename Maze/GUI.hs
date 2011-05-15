module Maze.GUI
where

-- TODO export only mazeGUI, hide others

import qualified Array as A
import qualified Data.Vector as V

import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.List ((\\))
import Data.Maybe (fromJust)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (Point)
import System.Random

import Maze.Maze
import Maze.Types
import Maze.Plan

{-
Used to construct and update the GUI.
-}

{-
Global constant values.
-}
gTITLE = "Robot in a maze"
gLOGO = "res/icon.png"
gTIME = 100

{-
Type of the ListStore used in GUI.
-}
type ListStoreType = (Int, Fitness)

{-
Type of finish information:
  * True/False depending on whether a chromosome was completely simulated.
  * Fitness of completed chromosome or 0 otherwise
-}
type FinishInfo = (Bool, Int, Fitness)

-- simulation not finished
notFinished :: FinishInfo
notFinished = (False, 0, 0)

-- simulation ended
end :: IORType -> FinishInfo
end r = (True, cGuy r,
  fitness (guyPos r) (guyTime r) (endPoint r) (endTime r) (guysBestDist r))

{-
Type of the IORef used.
-}
data IORType = IORCT
  { maze :: Maybe Maze
  , endPoint :: Point
  , endTime :: Time
  , cGuy :: Int
  , guyPos :: Point
  , guyTime :: Time
  , guysBestDist :: Int
  , plans :: V.Vector Plan
  , gen :: Maybe StdGen
  , model :: Maybe (ListStore ListStoreType)
  , cb :: Maybe HandlerId
  , bestFitness :: Int
  , generation :: Int
  , mRate :: Double
  }
empty = IORCT Nothing (0, 0) 0 0 (0, 0) 0 1000 V.empty Nothing Nothing Nothing (-100) 0 0.0

{-
Real evolution function. Will update IORType record.
-}
evolveFunc :: IORType -> (IORType, FinishInfo)
evolveFunc r@(IORCT
  { maze = Just m
  , endPoint = endp
  , endTime = endt
  , cGuy = guy
  , guyPos = pos
  , guyTime = t
  , guysBestDist = bd
  , plans = ps
  })
  -- normal case: in the middle of simulation
  | t < endt && pos /= endp = let (p, t') = doStep (m, ps V.! guy) (pos, t)
    in (r
      { guyPos = p
      , guyTime = t'
      , guysBestDist = min bd $ manhattan p endp
      } , notFinished)
  -- simulation ended
  | t == endt || pos == endp = (r
    { guyPos = (1, 1)
    , guyTime = 0
    , cGuy = guy + 1
    , guysBestDist = (snd endp) * (snd endp)}, end r)

{-
Evolution.
-}
evolve :: IORef IORType -> Label -> Label -> Label -> DrawingArea -> IO Bool
evolve ref gl csl fl dw = do
  -- 1. Update IORef
  r <- readIORef ref
  let (r', fI) = evolveFunc r
  writeIORef ref r'
  -- 2. If finished one step, update info
  let m = fromJust . model $ r' -- surely, it will exist
  let l = V.length . plans $ r'
  case fI of
    (True, i, f) -> do
      listStoreSetValue m i (i + 1, f)
      if i == l - 1
      then do
        finishStep ref
      else return ()
    _ -> return () -- ignore
  -- 3. Invalidate drawing area and draw
  (w, h) <- widgetGetSize dw
  widgetQueueDrawArea dw 0 0 w h
  -- 4. Update labels, after reading again the IORef
  r <- readIORef ref
  labelSetText gl $ show $ generation r
  labelSetText fl $ show $ bestFitness r
  labelSetText csl $ show (cGuy r, guyTime r)
  -- 5. Return
  return True

{-
Updates the IORef when an epoch is finished. Gets the best fitness so far,
updates the population, restart a new epoch.
-}
finishStep :: IORef IORType -> IO ()
finishStep ref = do
  -- 1. Read IORef
  r <- readIORef ref
  -- 2. Update best fitness, generation, and other simple values
  let ls = fromJust . model $ r
  l0 <- listStoreToList ls
  let l = map snd l0
  let m = maximum l
  let f = bestFitness r
  let m' = max m f
  let newGen = 1 + generation r
  -- 3. Get new population by mutation and crossover
  let oldPop = V.zip (plans r) (V.fromList l)
  let (plans', g') = runState (newPopulation oldPop (mRate r)) (fromJust $ gen r)
  -- 4. Clear the ListStore
  mapM_ (\x -> listStoreSetValue ls x (x+1, 0)) [0 .. length l - 1]
  -- 5. Create the new IORef
  writeIORef ref $ r
    { cGuy = 0
    , guyPos = (1, 1)
    , guyTime = 0
    , gen = Just g'
    , plans = plans'
    , bestFitness = m'
    , generation = newGen
    }

{-
Main window loop.
-}
mazeGUI = do
  -- 1. Get empty IORef
  ref <- newIORef $ empty
  -- 2. Init GTK
  initGUI
  window <- windowNew
  -- 3. Create the delete event allowing the app to finish
  window `on` deleteEvent $ liftIO mainQuit >> return False
  -- 3. Populate and set window's attributes.
  pbuff <- pixbufNewFromFile gLOGO
  set window
    [ windowDefaultWidth := 800
    , windowDefaultHeight := 600
    , windowTitle := gTITLE
    , windowIcon := Just pbuff
    ]
  drawing <- populateWindow window ref
  -- 4. Show everything
  widgetShowAll window
  -- 5. Set the drawing callbacks
  onExpose drawing $ \x -> do
    (w, h) <- widgetGetSize drawing
    drw <- widgetGetDrawWindow drawing
    c <- readIORef ref
    renderWithDrawable drw $ drawMaze (fromIntegral w) (fromIntegral h) c
    return False
  -- 6. Run GUI loop
  mainGUI

{-
Constructs all widgets found in the window.
-}
populateWindow :: Window -> IORef IORType -> IO DrawingArea
populateWindow w r = do
  -- 1. Build one vertical box for splitting the screen in two
  vBox <- vBoxNew False 10
  w `containerAdd` vBox
  -- 2. Build the main area
  (dw, gl, csl, fl) <- buildMainArea vBox r
  -- 3. Build the toolbar
  buildToolbar vBox r dw gl csl fl
  return dw

{-
Builds the application's toolbar.
-}
buildToolbar :: VBox -> IORef IORType -> DrawingArea -> Label -> Label -> Label -> IO ()
buildToolbar b r dw gl csl fl = do
  -- 1. Build toolbar and set attributes
  tb <- toolbarNew
  boxPackStart b tb PackNatural 10
  -- 2. Build tooltips collection
  tp <- tooltipsNew
  -- 3. Add widgets
  let addF = addBtnToToolbar tb tp -- helper function
  bNew <- addF stockNew "Starts a new population, with a new maze"
  bNew `onToolButtonClicked` (onNew r dw gl csl fl)
  bAbout <- addF stockAbout "About this program"
  bAbout `onToolButtonClicked` onAbout
  addSeparator tb
  return ()

{-
Adds a button to a toolbar, setting the tooltip and look as given by the
caller. Returns the new button, for callback setup or other uses.
-}
addBtnToToolbar :: Toolbar -> Tooltips -> StockId -> String -> IO ToolButton
addBtnToToolbar tb tp s tpString = do
  b <- toolButtonNewFromStock s
  toolItemSetTooltip b tp tpString ""
  toolbarInsert tb b $ -1
  return b

{-
Adds a separator to a toolbar.
-}
addSeparator :: Toolbar -> IO ()
addSeparator tb = do
  s <- separatorToolItemNew
  toolbarInsert tb s $ -1

{-
Builds the application main area.
-}
buildMainArea :: VBox -> IORef IORType -> IO (DrawingArea, Label, Label, Label)
buildMainArea b r = do
  -- 1. New HBox
  box <- hBoxNew False 10
  boxPackEnd b box PackGrow 10
  -- 2. Build statistics table
  (gl, csl, fl) <- buildPopulationInfo box r
  -- 3. Build maze area
  da <- buildMazeArea box
  -- 4. Return widgets to be completed in callbacks
  return (da, gl, csl, fl)

{-
Builds the population info table
-}
buildPopulationInfo :: HBox -> IORef IORType -> IO (Label, Label, Label)
buildPopulationInfo b r = do
  -- 1. A nice frame
  f <- frameNew
  f `frameSetLabel` "Statistics"
  boxPackEnd b f PackNatural 10
  -- 2. Another VBox
  box <- vBoxNew False 5
  f `containerAdd` box
  -- 3. A table
  t <- tableNew 3 2 True
  boxPackStart box t PackNatural 0
  -- 4. Current generation number display
  l <- labelNew $ Just "Generation:"
  tableAttachDefaults t l 0 1 0 1
  gl <- labelNew $ Just "0"
  tableAttachDefaults t gl 1 2 0 1
  -- 5. Current time step, guy number
  l' <- labelNew $ Just "Current step:"
  tableAttachDefaults t l' 0 1 1 2
  csl <- labelNew $ Just "(0, 0)"
  tableAttachDefaults t csl 1 2 1 2
  -- 6. Fitness display
  l'' <- labelNew $ Just "Best fitness:"
  tableAttachDefaults t l'' 0 1 2 3
  fl <- labelNew $ Just "0"
  tableAttachDefaults t fl 1 2 2 3
  -- 7. Population display
  buildPopulationDisplay box r
  -- 8. Return widgets to be completed in callbacks
  return (gl, csl, fl)

{-
Builds the tree view for population display.
-}
buildPopulationDisplay :: VBox -> IORef IORType -> IO ()
buildPopulationDisplay b ref = do
  -- 1. The model
  model <- listStoreNew [] -- empty for now
  r <- readIORef ref
  writeIORef ref $ r {model = (Just model)}
  -- 2. The view
  view <- treeViewNewWithModel model
  treeViewSetHeadersVisible view True
  boxPackStart b view PackGrow 0
  -- 3. Columns, accessors, etc
  buildColumn model view (show.fst) "Chromosome number"
  buildColumn model view (show.snd) "Fitness"

{-
Builds one column for the tree view.
-}
buildColumn :: ListStore a -> TreeView -> (a -> String) -> String -> IO ()
buildColumn model view f title = do
  c <- treeViewColumnNew
  cr <- cellRendererTextNew
  treeViewColumnPackStart c cr False
  cellLayoutSetAttributes c cr model $ \x -> [cellText := f x ]
  c `treeViewColumnSetTitle` title
  view `treeViewAppendColumn` c
  return ()

{-
Builds the area where the maze will be drawn.
-}
buildMazeArea :: HBox -> IO DrawingArea
buildMazeArea b = do
  -- 1. Get a frame around the maze
  f <- aspectFrameNew 0.5 0.5 Nothing
  f `frameSetLabel` "Maze"
  boxPackStart b f PackGrow 10
  -- 2. Return the canvas where the maze will be drawn
  canvas <- drawingAreaNew
  f `containerAdd` canvas
  return canvas

{-
The actual drawing of the maze.
-}
drawMaze :: Double -> Double -> IORType -> Render()
drawMaze w h (IORCT {maze = Nothing}) = do
  clean
  moveTo 0 0
  lineTo w h
  moveTo 0 h
  lineTo w 0
  stroke
drawMaze w h (IORCT {maze = Just m, guyPos = p}) = do
  clean
  let size = snd . snd . A.bounds $ m
  let fIs = fromIntegral size
  let dx = w / fIs
  let dy = h / fIs
  mapM_ (drawWalls m size dx dy) (A.indices m)
  stroke
  drawGuy p dx dy

{-
Draws the robot if it exists
-}
drawGuy :: Point -> Double -> Double -> Render ()
drawGuy (y, x) dx dy = do
  setSourceRGB 1 0 0
  let dx' = 0.2 * dx
  let dy' = 0.2 * dy
  let x' = dx * fromIntegral x - dx'
  let y' = dy * fromIntegral y - dy'
  let x'' = x' - dx + 2 * dx'
  let y'' = y' - dy + 2 * dy'
  moveTo x' y'
  lineTo x'' y''
  moveTo x'' y'
  lineTo x' y''
  stroke

{-
Cleanup of drawing area.
-}
clean :: Render()
clean = do
  setSourceRGB 1 1 1
  paint
  setSourceRGB 0 0 0

{-
Draws wall for a single cell.
-}
drawWalls :: Maze -> Int -> Double -> Double -> Point -> Render ()
drawWalls m s dx dy p@(x, y) = mapM_ (renderOneWall dx dy y' x') walls
  where
    x' = dx * (fromIntegral x) + if x == 1 then 1 else if x == s then -1 else 0
    y' = dy * (fromIntegral y) + if y == 1 then 1 else if y == s then -1 else 0
    frees = (\(C l) -> l) $ m A.! p
    l = if y == 1 then if x == 1 then [] else [N] else [W]
    walls = ([N, E, S, W] \\ frees) \\ l

{-
Renders a single wall.
-}
renderOneWall :: Double -> Double -> Double -> Double -> Cardinal -> Render ()
renderOneWall dx dy x y N = do
  moveTo x (y - dy)
  lineTo (x - dx) (y - dy)
renderOneWall dx dy x y E = do
  moveTo x y
  lineTo x (y - dy)
renderOneWall dx dy x y S = do
  moveTo x y
  lineTo (x - dx) y
renderOneWall dx dy x y W = do
  moveTo (x - dx) y
  lineTo (x - dx) (y - dy)

{-
Action to do when clicking the New button.
-}
onNew :: IORef IORType -> DrawingArea-> Label -> Label -> Label -> IO ()
onNew ref dw gl csl fl = do
  -- 1. Present config dialog and get options TODO
  let popSize = 10
  let mRate = 0.1
  -- 2. Get maze
  let (maze, g) = runState (genMaze (3, 3)) (mkStdGen 42)
  -- 3. Fill ListStore from IORef
  r <- readIORef ref
  fillListStore (model r) popSize
  -- 4. Get maze details
  let size = snd . snd . A.bounds $ maze
  let pLen = size * size
  -- 4. Get initial plans
  let (plans, g') = runState (getRandomInitialPlans popSize pLen) g
  -- 5. Invalidate drawing area
  (w, h) <- widgetGetSize dw
  widgetQueueDrawArea dw 0 0 w h
  -- 6. Setup timer callback
  case cb r of
    Just cb -> timeoutRemove cb
    Nothing -> return ()
  cb <- timeoutAdd (evolve ref gl csl fl dw) gTIME
  -- 7. Add everything to IORef
  writeIORef ref $ r
    { maze = Just maze
    , endPoint = (size, size)
    , endTime = pLen
    , gen = Just g'
    , cb = Just cb
    , cGuy = 0
    , guyPos = (1, 1)
    , guyTime = 0
    , plans = plans
    , bestFitness = -10000
    , generation = 1
    , mRate = mRate
    }

{-
Completes the list store with the initial population.
-}
fillListStore :: Maybe (ListStore ListStoreType) -> Int -> IO ()
fillListStore Nothing _ = return ()
fillListStore (Just m) s = do
  listStoreClear m
  mapM_ (\x -> listStoreAppend m (x, 0)) [1..s]

{-
Action to do when clicking the about button.
-}
onAbout :: IO ()
onAbout = do
  d <- aboutDialogNew
  pbuff <- pixbufNewFromFile gLOGO
  set d
    [ aboutDialogProgramName := gTITLE
    , aboutDialogVersion := "0.1"
    , aboutDialogAuthors := ["Mihai Maruseac <mihai.maruseac@rosedu.org>"]
    , aboutDialogDocumenters := ["Mihai Maruseac <mihai.maruseac@rosedu.org>"]
    , aboutDialogArtists := ["Art taken from Public Domain on the web"]
    , aboutDialogComments := "Uses genetic algorithms to evolve a robot in a maze \nSee README and LICENSE for more information."
    , aboutDialogCopyright := "Copyright © 2011 - 2012 Mihai Maruseac <mihai.maruseac@rosedu.org>"
    , aboutDialogLogo := Just pbuff
    , windowIcon := Just pbuff
    ]
  dialogRun d
  widgetDestroy d

