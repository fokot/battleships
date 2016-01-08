module Battleships.Game where



import System.Random  -- for randoms
import System.IO      -- for hFlush
import Prelude 
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe


type Unit = Int
type Coord = (Unit, Unit)

--Record:

nr_cols, nr_lines :: Int  -- maximum value!
nr_cols = 10
nr_lines = 10

boats :: [Unit]
boats = [5,4,3,2,2,1,1]

dim :: Int
dim = 10

type Grid = Map Coord Cell

emptyGrid :: Grid
emptyGrid = fromList [((x,y), Empty) | x <- [1..dim], y <- [1..dim]]

computerGridTest :: IO Grid
computerGridTest = return $ fromList [((x,y), if(x*y `rem` 3 == 0) then Empty else Boat ) | x <- [1..dim], y <- [1..dim]]

-- one shot
shootComputer :: Grid -> IO Coord
shootComputer g =  return $ fst (head (toList (Map.filter notShot g)))
  where notShot Boat = True
        notShot Empty = True
        notShot _ = False

{-
-- computer shoots until it misses        
shootingComputer :: Grid -> IO [(Coord, Cell)]
shootingComputer g = let f g cs = do shot <- shootComputer g
                                     if look shot g /= BoatShot
                                      then return ((shot, EmptyShot):cs)
                                      else f (insert shot BoatShot g) ((shot, BoatShot):cs)
                     in f g []-}
        
        


look k m = a where (Just a) = Map.lookup k m                


-- new cell state after shot
shootToCell :: Coord -> Grid -> Cell
shootToCell c g = case look c g of
                       Boat  -> BoatShot
                       Empty -> EmptyShot
                       _     -> error "Already shot"
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
-- Computer shot



type Row = [Int]

                   
-- one more constructor added ; so if the ship is Entirly taken down it should be marked so. "DeadShip"
data Cell = Boat | Empty | BoatShot | EmptyShot | DeadShip
  deriving (Show, Eq)

-- we need functions to check the state of the cell ...

-- this function will check if the cell is empty
isEmpty ::  Cell -> Bool
isEmpty Empty = True
isEmpty Boat = True 
isEmpty _ = False

-- this function will check if the cell is EmptyShot
isEmptyShot :: Cell -> Bool
isEmptyShot EmptyShot = True
isEmptyShot _ = False

-- this function will check if the cell is Boat
isBoat ::  Cell -> Bool
isBoat Boat = True
isBoat _ = False

-- this function will check if the cell is a BoatShot
isBoatShot ::  Cell -> Bool
isBoatShot BoatShot = True
isBoatShot _ = False

-- types
type Pos = (Int, Int)
type Dir = Pos

-- checks if the (x,y) is within the Grid range
validRange :: Pos -> Grid -> Bool
validRange (x,y) g = nr_lines >= x && x>= 1 &&  nr_cols >= y && y >= 1

-- returns a list of all neighbours of a Cell
neighbours :: Pos -> [Pos]
neighbours (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

-- returns a list of all valid neighbours of a Cell
validNeighbours :: Pos -> Grid -> [Pos]
validNeighbours p g = List.filter (\ q -> validRange q g) (neighbours p) 

-- returns a list of positions of Cells filtered with a certain criteria
getPositions :: (Cell -> Bool) -> Grid -> [Pos]
getPositions p g = Map.keys (Map.filter p g)

-- returns a list of all BoatShot-Cells
getBoatShots = getPositions isBoatShot

-- returns a list of all Empty-Cells
getEmpties = getPositions isEmpty

-- computes the direction from a position and a position
posDiff :: Pos -> Pos -> Dir
posDiff (x1,y1) (x2,y2) = (x1-x2,y1-y2)

-- computes the position from a position and a Direction
posAdd :: Pos -> Dir -> Pos
posAdd (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- gets the status of the cell giving the cell's position in the grid
gridAt :: Grid -> Pos -> Cell
gridAt g p = g Map.! p


-- it looks for the next empty cell on the grid, and returns its position. or Nothing otherwise.
-- Nothing is returned if the position is of an invalid range or if it's an EmptyShot
nextEmpty :: Dir -> Pos -> Grid -> Maybe Pos 
nextEmpty dir pos grid 
  | not (validRange pos grid)     = Nothing
  | isEmptyShot (gridAt grid pos) = Nothing
  | isEmpty (gridAt grid pos)     = Just pos
  | otherwise                     = nextEmpty dir (posAdd pos dir) grid


-- it returns a random element of a list
randomFromList :: [a] -> IO a
randomFromList xs = do r <- randomRIO (0,length xs-1)
                       return (xs !! r)

-- it will return the position of a random empty cell
randomMove :: Grid -> IO Pos
randomMove grid = randomFromList (getEmpties grid)

--it returns the position the computer is supposed to hit next
computeMove :: Grid -> IO Pos
computeMove g = computeMove_aux g (getBoatShots g)
 

--it returns the position the computer is supposed to hit next
computeMove_aux g ll= case ll of -- a list of BoatShot cells 
      []    -> randomMove g -- if the list is empty then it would be a random move
  
    -- else ; which means we have at least 1 element in the list that is a BoatShot
      -- then we see if there is a BoatShot neighbour of that position
      (p:l) -> case List.filter (\ q -> isBoatShot (gridAt g q)) (validNeighbours p g) of
          -- if there is not a neighbour that is a BoatShot; then return a random neighbour             -- that is valid
          [] -> case (List.filter (\ q -> isEmpty (gridAt g q)) (validNeighbours p g)) of
              [] -> computeMove_aux g l
              _ -> randomFromList (List.filter (\ q -> isEmpty (gridAt g q)) (validNeighbours p g))

          -- else; there is neighbour that is a BoatShot, then return the position of the next            -- hit that is on the same line
          (p':_) -> case (maybeToList (nextEmpty (posDiff p p') p  g) ++
                                                                maybeToList (nextEmpty (posDiff p' p) p' g)) of
              [] -> computeMove_aux g l
              (a:_) -> return (head (maybeToList (nextEmpty (posDiff p p') p  g) ++
                                                                maybeToList (nextEmpty (posDiff p' p) p' g))) 


-- true if this board finished
finished :: Grid -> Bool
finished g = size (Map.filter (==Boat) g) == 0            

-- Functions for testing:

testGrid :: Grid
testGrid = (Map.insert (10,5) BoatShot
           (Map.insert (3,2) BoatShot 
           (Map.insert (3,1) BoatShot emptyGrid)))


