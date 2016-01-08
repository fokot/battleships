
module Battleships.GUI (
  main
) 

where

import Graphics.UI.Gtk hiding (Boat)
import Graphics.UI.Gtk.Gdk.EventM
import Data.Map hiding (filter, map)
import Data.List hiding (insert)
import Data.IORef
import Control.Monad.Trans
import Control.Monad
import Battleships.Game
import Gtk2hs.Utils
import Battleships.Placement


-- Images & Utils
toImageName :: Cell -> String
toImageName c = (show c) ++ ".png"

pixs :: [(Cell, IO Pixbuf)]
pixs = map (\x -> (x, pixbufNewFromFile $ toImageName x)) cells
        where cells = [Boat, Empty, BoatShot, EmptyShot]

pix :: Cell -> IO Pixbuf
pix c = let (Just (_, img)) = find  (\(cell, _) -> cell == c) pixs
        in img
        
scale :: Pixbuf -> Int -> Int -> IO Pixbuf 
scale p wScale hScale= do w <- pixbufGetWidth p
                          h <- pixbufGetHeight p
                          pixbufScaleSimple p (w*wScale) (h*hScale) InterpNearest
                          
newImageInEventBox :: Pixbuf -> IO EventBox
newImageInEventBox pb = do eb <- eventBoxNew
                           img <- imageNewFromPixbuf pb
                           set eb[containerChild := img, containerBorderWidth := 0 ]
                           return eb

getPlayersImage :: Cell -> IO Image
getPlayersImage cell = pix cell >>= imageNewFromPixbuf

-- hiding
getComputersImage :: Cell -> IO Image
getComputersImage cell = getComputersPixbuf cell >>= imageNewFromPixbuf
                         
-- hiding
getComputersPixbuf :: Cell -> IO Pixbuf
getComputersPixbuf cell = if cell == Boat || cell == Empty 
                          then do pix Empty
                          else do pix cell


-- Main function
main :: IO ()
main = do
  initGUI

  -- window
  window <- windowNew
  onDestroy window mainQuit   

  -- main vertical box
  vbox       <- vBoxNew False 0
  set window [containerBorderWidth := 10,
              windowTitle := "Ships",
              containerChild := vbox]


  -- box for tables
  hboxTables <- hBoxNew False 0
  boxPackStart vbox hboxTables PackNatural 0

  -- box for ships placement
  vboxShips <- vBoxNew False 0
  boxPackStart hboxTables vboxShips PackNatural 0

  -- ships for placement
  let f n = do q <- newBoatForPlacement n
               boxPackStart vboxShips q PackGrow 0
  mapM_ f boats
  
  
  -- players table        
  vboxPlayer <- vBoxNew False 0
  boxPackStart hboxTables vboxPlayer PackNatural 0
  
  labelPlayer <- labelNew (Just "Player")
  boxPackStart vboxPlayer labelPlayer PackNatural 0
  
  tablePlayer <- tableNew 2 2 True
  containerSetBorderWidth tablePlayer 10
  boxPackStart vboxPlayer tablePlayer PackNatural 0


  pt <- createSynchronizedTable tablePlayer emptyGrid playerImage
  redraw pt

  -- computers table
  vboxComputer <- vBoxNew False 0
  boxPackStart hboxTables vboxComputer PackNatural 0
  
  labelComputer <- labelNew (Just "Mr. Haskell")
  boxPackStart vboxComputer labelComputer PackNatural 0
 

  tableComputer <- tableNew 2 2 True
  containerSetBorderWidth tableComputer 10
  boxPackStart vboxComputer tableComputer PackNatural 0
  
  cg <- computerGrid  
  ct <- createSynchronizedTable tableComputer cg (computerImage pt)
  redraw ct 


  -- separator
  sep     <- hSeparatorNew
  boxPackStart vbox sep PackNatural 0

  -- box for buttons
  vboxButtons <- vBoxNew False 0
  boxPackStart vbox vboxButtons PackRepel 0
  containerSetBorderWidth vboxButtons 10
  
  -- new game button
  buttonNewGame <- buttonNewWithLabel "New Game"
  onClicked buttonNewGame (newGame pt ct)
  boxPackStart vboxButtons buttonNewGame PackGrow 0

  -- quit button
  buttonQuit <- buttonNewWithLabel "Quit"
  onClicked buttonQuit mainQuit
  boxPackStart vboxButtons buttonQuit PackGrow 0

  widgetShowAll window
  mainGUI


showPopup :: String -> IO ()
showPopup msg = do
  d <- dialogNew
  b <- dialogAddButton d "Close" ResponseClose
  dvb <- dialogGetUpper d
  l <- labelNew (Just msg)
  boxPackStart dvb l PackGrow 0
  widgetShowAll dvb
  onClicked b (widgetHide d)
  dialogRun d
  return ()

-- creates one boat for placement  
newBoatForPlacement size = do
  pbSmall <- pix Boat
  pb <- scale pbSmall size 1

  eb <- newImageInEventBox pb

  -- drag and drop setup
  dragSourceSet eb [Button1] [ActionMove]
  dragSourceAddTextTargets eb
  eb `on` dragDataGet $ \ ctx ii ts -> selectionDataSetText (show size) >> return () 
  eb `on` dragDataDelete $ \ctx -> widgetSetSensitive eb False
  dragSourceSetIconPixbuf eb pb
  return eb


playerImage :: SynchronizedTable Cell  -> Cell -> Int -> Int -> IO Widget
playerImage syncTable cell x y = do
 i <- getPlayersImage cell
 dragDestSet i [DestDefaultAll] [ActionMove]
 dragDestAddTextTargets i
 i `on` dragDataReceived 
           $ \ctx p ii ts -> do (Just dataSent) <- selectionDataGetText                      
                                let size = read dataSent :: Int
                                let coordinates = [(x+xs,y) | xs<-[0..(size-1)] ]  
                                let f coord = liftIO (Gtk2hs.Utils.changeCellState syncTable coord Boat)
                                mapM_ f coordinates
                                return ()
 return (toWidget i)

computerImage :: SynchronizedTable Cell -> SynchronizedTable Cell -> Cell -> Int -> Int -> IO Widget
computerImage syncTablePlayer syncTable cell x y = do
 eb <- eventBoxNew
 i <- getComputersImage cell
 set eb[containerChild := i, containerBorderWidth := 0 ]
 onButtonPress eb (shoot i (x,y) syncTable syncTablePlayer)
 return (toWidget eb)

 
         
         
         

-- called when user press new game button
newGame :: SynchronizedTable Cell -> SynchronizedTable Cell -> IO ()
newGame pt ct = do setState pt emptyGrid     
                   cg <- computerGrid  
                   setState ct cg


-- called when user shoots
shoot :: Image -> Coord -> SynchronizedTable Cell -> SynchronizedTable Cell -> t -> IO Bool
shoot i coord cSyncTable pSyncTable _ = do
  cgOld <- readIORef (ref cSyncTable)
 
  let cellOld = look coord cgOld
  if cellOld == EmptyShot ||  cellOld == BoatShot
   then return True -- if user clicks on shot position, nothing happens
   else do
    -- updating grid
    let newCell =  shootToCell coord cgOld

    Gtk2hs.Utils.changeCellState cSyncTable coord newCell
    
    cg <- readIORef (ref cSyncTable)
    if finished cg then showPopup "Unbelievable! You won!" else return ()
    
    -- if player hits computer does not shoot
    if newCell == BoatShot
     then return True
     else do
       -- shoooting computer
       shootingComputer pSyncTable
       return True

shootingComputer :: SynchronizedTable Cell -> IO ()
shootingComputer pSyncTable = do 
  pg <- readIORef (ref pSyncTable)
  coord <- computeMove pg
  let newCell = shootToCell coord pg
  Gtk2hs.Utils.changeCellState pSyncTable coord newCell
  
  pg <- readIORef (ref pSyncTable)
  
  if finished pg then showPopup "Mr Haskell beated you!" else
   when (newCell == BoatShot) (shootingComputer pSyncTable)
