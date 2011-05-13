module Main
where

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk

{- TODO move -}
main = do
  {- first, read arguments: TODO -}
  loopGUI

{-
Used to construct and update the GUI.
-}

{-
Main window loop.
-}
loopGUI = do
  -- 1. Init GTK
  initGUI
  window <- windowNew
  -- 2. Create the delete event allowing the app to finish
  window `on` deleteEvent $ liftIO mainQuit >> return False
  -- 3. Populate and set window's attributes.
  populateWindow window
  set window
    [ windowDefaultWidth := 200
    , windowDefaultHeight := 200
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
  addF stockNew "Starts a new population, with a new maze"
  addF stockAbout "About this program"
  addSeparator tb
  return ()

{-
Adds a button to a toolbar, setting the tooltip and the callback as given to
this function. Returns the new button, in case of application wanting to do
something more than this function does.
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

