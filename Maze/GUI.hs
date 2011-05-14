module Maze.GUI
where

import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import System.Random

import Maze.Maze

{-
Used to construct and update the GUI.
-}

gTITLE = "Robot in a maze"
gLOGO = "res/icon.png"

{-
Main window loop.
-}
mazeGUI = do
  -- 1. Init GTK
  initGUI
  window <- windowNew
  -- 2. Create the delete event allowing the app to finish
  window `on` deleteEvent $ liftIO mainQuit >> return False
  -- 3. Populate and set window's attributes.
  pbuff <- pixbufNewFromFile gLOGO
  set window
    [ windowDefaultWidth := 400
    , windowDefaultHeight := 300
    , windowTitle := gTITLE
    , windowIcon := Just pbuff
    ]
  drawing <- populateWindow window
  -- 4. Show everything
  widgetShowAll window
  -- 5. Set the drawing callbacks TODO
  -- 6. Run GUI loop
  mainGUI

{-
Constructs all widgets found in the window.
-}
populateWindow :: Window -> IO DrawingArea
populateWindow w = do
  {-
  1. Build one vertical box for splitting the screen in two and add it to
  window. The areas obtained are separated by 10 pixels and are not equal in
  height.
  -}
  vBox <- vBoxNew False 10
  w `containerAdd` vBox
  -- 2. Build the toolbar
  buildToolbar vBox
  -- 3. Build the main area
  buildMainArea vBox

{-
Builds the application's toolbar.
-}
buildToolbar :: VBox -> IO ()
buildToolbar b = do
  -- 1. Build toolbar and set attributes
  tb <- toolbarNew
  boxPackStart b tb PackNatural 10
  -- 2. Build tooltips collection
  tp <- tooltipsNew
  -- 3. Add widgets
  let addF = addBtnToToolbar tb tp -- helper function
  bNew <- addF stockNew "Starts a new population, with a new maze"
  bNew `onToolButtonClicked` onNew
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
buildMainArea :: VBox -> IO DrawingArea
buildMainArea b = do
  -- 1. New HBox
  box <- hBoxNew False 10
  boxPackStart b box PackGrow 10
  -- 2. Build statistics table
  buildPopulationInfo box
  -- 3. Build maze area
  buildMazeArea box

{-
Builds the population info table
-}
buildPopulationInfo b = do
  -- 1. A nice frame
  f <- frameNew
  f `frameSetLabel` "Statistics"
  boxPackEnd b f PackNatural 10
  -- 2. Another VBox
  box <- vBoxNew False 5
  f `containerAdd` box
  -- 3. A table
  t <- tableNew 2 2 True
  boxPackStart box t PackNatural 0
  -- 4. Current generation number display
  l <- labelNew $ Just "Generation:"
  tableAttachDefaults t l 0 1 0 1
  gl <- labelNew $ Just "0"
  tableAttachDefaults t gl 1 2 0 1
  -- 5. Fitness display
  l' <- labelNew $ Just "Best fitness:"
  tableAttachDefaults t l' 0 1 1 2
  fl <- labelNew $ Just "0"
  tableAttachDefaults t fl 1 2 1 2
  -- 6. Population display
  buildPopulationDisplay box

{-
Builds the tree view for population display.
-}
buildPopulationDisplay b = do
  -- 1. The model
  model <- listStoreNew ([] :: [(Int, Int)])-- empty for now
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
drawMaze :: Render()
drawMaze = do
  return ()

{-
Action to do when clicking the New button.
-}
onNew :: IO ()
onNew = do
  -- 1. present config dialog and get options TODO
  -- 2. get maze
  let (maze, g) = (runState $ genMaze (5, 5)) (mkStdGen 42)
  print maze

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

