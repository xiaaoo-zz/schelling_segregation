module Schelling
  ( Coord
  , AgentType (..)
  , Cell

  , step
  ) where

import System.Random

-- type-definition of a coordinate in our world
type Coord = (Int, Int)

-- data definition for the agent types
data AgentType 
  = Red       -- ^ Red agent
  | Green     -- ^ Green agent
  | Blue      -- ^ Blue agent
  deriving (Eq, Show) -- Needed to compare for equality, otherwise would need to implement by ourself

-- Type-definition of a Cell: it is just a coordinate and an optional AgentType.
-- Note: the agent type is optional and is Nothing in case the cell is empty.
type Cell = (Coord, Maybe AgentType)

-- Computes one step of the Schelling Segregation model.
step :: [Cell]           -- ^ All cells of the world
     -> StdGen           -- ^ The random-number generator
     -> Double           -- ^ The ratio of equal neighbours an agent requires to be happy
     -> ([Cell], StdGen) -- ^ Result is the new list of cells and the updated random-number generator
step cs g ratio = (cs', g')
  where
    -- filter all empty cells
    csEmpty = filter isEmpty cs

    -- filter all agents
    csNonEmpty = filter (not . isEmpty) cs
    -- filter all unhappy agents
    csUnhappy = filter (not . isHappy ratio cs) csNonEmpty
    -- filter all happy agents
    csHappy = filter (isHappy ratio cs) csNonEmpty
    
    -- Move the unhappy agents to a random empty cell. Note we use fold(r) for 
    -- this because we need to update data while iterating over the unhappy
    -- agents: cells can become empty or occupied.
    (csUnhappy', csEmpty', g') = foldr moveCell ([], csEmpty, g) csUnhappy 

    -- create a new list of cells for the result of this step:
    -- the happy agents (which have not moved) plus the updated unhappy agents
    -- plus the updated empty cells
    cs' = csHappy ++ csUnhappy' ++ csEmpty'

    -- Returns True if a cell is empty
    isEmpty :: Cell -> Bool
    isEmpty (_, Nothing) = True
    isEmpty _            = False

-- Returns True if an agent on a given cell is happy or not
isHappy :: Double  -- ^ The satisfaction factor
        -> [Cell]  -- ^ All cells
        -> Cell    -- ^ The cell with the agent
        -> Bool    -- ^ True in case the agent is happy, False otherwise
isHappy ratio cs c = (agent_ratio >= ratio)
  -- where
    -- TODO implement
    -- 1. compute neighbour coordinates
    -- 2. get all cells for the neighbour coordinates
    -- 3. compute the ratio (include empty cells!)
    where
      agent_ratio = (fromIntegral (length same_color_cells) / fromIntegral (length all_cells))

      all_cells = gen_all_cells cs (genNeibors moore c)
      same_color_cells = gen_same_color_cells all_cells c
  
      moore = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
      genNeibors :: [Coord] -> Cell -> [Coord]
      genNeibors moores (coord,_) = [(x+a,y+b) | (x,y) <- [coord], (a,b) <- moores]

      gen_all_cells :: [Cell] -> [Coord] -> [Cell]
      gen_all_cells cells coords = filter (\(coord,_) -> coord `elem` coords) cells
  
      gen_same_color_cells :: [Cell] -> Cell -> [Cell]
      gen_same_color_cells cells (_,own_type) = filter (\(_,agent_type) -> agent_type == own_type) cells

-- Moves an unhappy agent to an empty cell.
-- The moved agent must be appended to the first list. the new empty cell must
-- be appended to the second list in the return tuple. The StdGen is used to
-- draw random numbers.
moveCell :: Cell                      -- ^ The unhappy agent to move
         -> ([Cell], [Cell], StdGen)  -- ^ The already move agents, the empty cells, the random-number generator
         -> ([Cell], [Cell], StdGen)  -- ^ The already move agents, the empty cells, the random-number generator
moveCell c (csm, cse, g) = (csm', cse', g')
  -- where
    -- TODO: implement
    -- 1. get a random empty cell (cse contains empty only)

    -- 2. swap the contents of the two cells:
    -- empty cell becomes occupied by agent
    -- occupied cell becomes empty

    -- 3. add newly occupied cell to the accumulator of occupied cells 

    -- 4. remove the previously empty cell, which is occupied now, from the list
    -- of empty cells
    where
      n = length cse - 1
      (ran, g') = randomR (0, n) g
      random_cell = cse !! ran
  
      -- filter out the random empty cell and add new empty cell
      cse' = filter (\ce -> ce /= random_cell) cse ++ [new_moved_cell c random_cell]

      -- add the moved cell
      csm' = csm ++ [new_moved_cell random_cell c]

      new_moved_cell :: Cell -> Cell -> Cell
      new_moved_cell (coord1,tp1) (coord2,tp2) = (coord1,tp2)

