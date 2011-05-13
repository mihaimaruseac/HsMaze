module Maze.GUI
where

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk

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
  populateWindow window
  set window
    [ windowDefaultWidth := 200
    , windowDefaultHeight := 200
    , windowTitle := gTITLE
    , windowIcon := Just pbuff
    ]
  -- 4. Show everything
  widgetShowAll window
  -- 5. Run GUI loop
  mainGUI

{-
Constructs all widgets found in the window.
-}
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
Action to do when clicking the New button.
-}
onNew = undefined

{-
Action to do when clicking the about button.
-}
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

