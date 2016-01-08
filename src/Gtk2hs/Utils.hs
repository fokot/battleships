
module Gtk2hs.Utils (
  createSynchronizedTable,
  SynchronizedTable,
  ref,
  changeCellState,
  setState,
  redraw
  ) where

import Graphics.UI.Gtk
import Data.Map
import Data.IORef
import Control.Monad.Trans


-- creates new SynchronizedTable and stores its widgets
createSynchronizedTable :: Table -> Map (Int, Int) a -> (SynchronizedTable a -> a -> Int -> Int -> IO Widget) -> IO (SynchronizedTable a)
createSynchronizedTable t r f = do ms <- newIORef r
                                   ws <- newIORef empty :: IO (IORef (Map (Int, Int) Widget))
                                   return (SynchronizedTable t ms f ws)

-- synchronizes Table with model
data SynchronizedTable a = SynchronizedTable { table :: Table,  
                                               ref :: IORef (Map (Int, Int) a), 
                                               createWidget :: SynchronizedTable a -> a -> Int -> Int -> IO Widget,
                                               widgets :: IORef (Map (Int, Int) Widget)}

-- changes the state of the grid and updates the widget in the table
changeCellState :: SynchronizedTable a -> (Int, Int) -> a -> IO ()
changeCellState syncTable (x,y) cell = do
  insertToIOMap (x,y) cell (ref syncTable)
  widget <- (createWidget syncTable) syncTable cell x y

  ws <- readIORef (widgets syncTable)
  let oldWidget = mapLook (x,y) ws
  containerRemove (table syncTable) oldWidget
  
  insertToIOMap (x,y) widget (widgets syncTable)
  
  tableAttachDefaults (table syncTable) widget (x-1) x (y-1) y
  widgetShowAll (table syncTable)
        
        
-- redraws the table
setState :: SynchronizedTable a -> Map (Int, Int) a -> IO ()
setState syncTable newState = do
  writeIORef (ref syncTable) newState
  let t  = table syncTable
  let drawOne ((x,y), cell) = do widget <- (createWidget syncTable) syncTable cell x y
                                 insertToIOMap (x,y) widget (widgets syncTable)
                                 tableAttachDefaults t widget (x-1) x (y-1) y
  mapM_ drawOne (toList newState)
  widgetShowAll t

-- redraws the table
redraw :: SynchronizedTable a -> IO ()
redraw syncTable = do
  m <- readIORef (ref syncTable)
  let t  = table syncTable
  let drawOne ((x,y), cell) = do widget <- (createWidget syncTable) syncTable cell x y
                                 insertToIOMap (x,y) widget (widgets syncTable)
                                 tableAttachDefaults t widget (x-1) x (y-1) y
  mapM_ drawOne (toList m)
  widgetShowAll (table syncTable)

-- lookup in map
mapLook k m = a where (Just a) = Data.Map.lookup k m 

-- inserts to IORef (Map k a)
insertToIOMap :: (Ord k) => k -> a -> IORef (Map k a) -> IO ()
insertToIOMap k a mRef = do 
  mOld <- readIORef mRef
  let mNew = Data.Map.insert k a mOld
  writeIORef mRef mNew
