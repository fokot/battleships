module Battleships.Placement where

import System.Random  -- for randoms
import System.IO      -- for hFlush
import Prelude 
import Data.Map as Map
import Data.Maybe as Maybe
import Battleships.Game 


computerGrid = placeShips boats emptyGrid


placeShips:: [Int] -> Grid -> IO Grid
placeShips [] board = return board
placeShips (ship:ships) board = do newBoard <- placeShip ship board
                                   placeShips ships newBoard

                                                  
gen2 x y =getStdRandom (randomR (x,y))

placeShip:: Int -> Grid ->IO Grid
placeShip ship board= do
                    x <- gen2 1 nr_lines
                    y <- gen2 1 nr_cols
                    d <- gen2 0 1
                    if not (goodPlacement ship d x y board) 
                         then placeShip ship board
                            else return (changeValue ship d x y board)



changeValue :: Int -> Int -> Int ->Int -> Grid -> Grid
changeValue len d x y g = (insertList [ (x1,y1) | x1 <- [0..nr_cols], y1 <- [0..nr_lines], ( (d==1)&&(y==y1)&&(x1>=x)&&(x1<x+len) )||( (d==0)&&(x==x1)&&(y1>=y)&&(y1<y+len) ) ] g)

insertList :: [(Int,Int)] -> Map (Int,Int) Cell -> Map (Int,Int) Cell 
insertList [] map = map
insertList ((x,y):rest) map = insertList rest (insert (x,y) Boat map) 

goodPlacement:: Int->Int->Int->Int->Grid->Bool              --- WORKING
-- length, direction, x and y position, board returns true or false
goodPlacement len dir x y board=
       len==0 || ((if dir==0 then (y+len-1<=nr_cols) && (goodPlacement (len-1) dir x (y+1) board)  -- direction horizontally, it doesn't surpass the board
                               else (x+len-1<=nr_lines) && (goodPlacement (len-1) dir (x+1) y board))  -- direction vertically, it doesn't surpass the board
                      && checkSquareAndNeighbour board x y )
                                                                                      


checkSquareAndNeighbour :: Grid -> Int -> Int -> Bool         ---WORKING
checkSquareAndNeighbour board y x = (valueOf board x y == Empty) && -- the square is empty
        not ( (y>1)&& ((valueOf board x (y-1))==Boat)) && -- the northern neighbour, if there is one, isn't already occupied by a ship
        not ( (y<nr_cols)&& ((valueOf board x (y+1))==Boat)) && -- the southern neighbour
        not ( (x<nr_lines)&& ((valueOf board (x+1) y)==Boat)) && -- the eastern neighbour
        not ( (x>1)  && ((valueOf board (x-1) y)==Boat)) -- the western neighbour


valueOf:: Grid -> Int -> Int -> Cell                      --it doesn't have an Error output!
valueOf g x y = findWithDefault Empty (y,x) g