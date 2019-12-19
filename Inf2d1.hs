-- Inf2d Assignment 1 2018-2019
-- Matriculation number:s1703913
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = []-- [(4,2),(4,3),(4,5),(5,1),(6,3),(2,2),(2,3),(3,4),(5,5)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth = (gridLength_search * gridWidth_search) - length badNodesList
-- Why did you choose this number?
-- I chose this formula because the map has this many free cells. Therefore, we know that if there is a solution, it must be of this length at the longest.

-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] =  []
next branch@((x,y):nds) = filter (not . null) (check (x+1, y) branch : check (x-1, y) branch : check (x, y-1) branch : [check (x, y+1) branch])

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode

-- Section 3 Uniformed Search

-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.
breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch destination next branches exploredList
  | null branches = Nothing -- no solution found
  | checkArrival destination (head pop_branch) = Just pop_branch
  | otherwise = breadthFirstSearch destination next queue exploredList
    where pop_branch = last branches -- check in the front (oldest inserted branches)
          queue =  next pop_branch ++ init branches -- new nodes go in front
-- NOTE: `exploredList` is redundant since the `next` function already checks for repeated nodes.
-- NOTE: I thought `exploredList` could be a list of explored branches, but then again it didn't make sense either.

-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next branches exploredList
  | null branches = Nothing -- no solution found
  | checkArrival destination (head pop_branch) = Just pop_branch
  | otherwise = depthFirstSearch destination next stack exploredList
    where pop_branch = head branches -- check in the back (newly inserted branches)
          stack =  next pop_branch ++ tail branches -- new nodes go in front
-- NOTE: `exploredList` is redundant since the `next` function already checks for repeated nodes.
-- NOTE: I thought `exploredList` could be a list of explored branches, but then again it didn't make sense either.

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch destination next branches d
  | null branches = Nothing -- no solution found
  | checkArrival destination (head pop_branch) = Just pop_branch
  | otherwise = depthLimitedSearch destination next stack d
  where pop_branch = head branches -- check in the back (newly inserted branches)
        stack =  expanded_branches ++ tail branches -- new nodes go in front
        expanded_branches = if length (head branches) <= d
                            then next (head branches)
                            else [] -- if past depth limit, solution not found

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
  | d > maxDepth = Nothing
  | answer_branch == Nothing = iterDeepSearch destination next initialNode (d+1) -- no solution found, increase depth limit
  | otherwise = answer_branch
  where answer_branch = depthLimitedSearch destination next [[initialNode]] d

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan (x, y) (a, b) = abs(x-a) + abs(y-b)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic branches exploredList
  | null branches = Nothing -- no solution found
  | checkArrival destination (head pop_branch) = Just pop_branch
  | otherwise = bestFirstSearch destination next heuristic queue exploredList
    where pop_branch = last branches -- check in the front (lowest heuristic value)
          priority_sort a b -- branches with heads having lower heuristic values go in the front of the queue (GT)
            | heuristic (head a) < heuristic (head b) = GT
            | otherwise = LT
          queue =  sortBy priority_sort (next pop_branch ++ init branches) -- always sort queue by priority
-- NOTE: `exploredList` is redundant since the `next` function already checks for repeated nodes.
-- NOTE: I thought `exploredList` could be a list of explored branches, but then again it didn't make sense either.


-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList
  | null branches = Nothing -- no solution found
  | checkArrival destination (head pop_branch) = Just pop_branch
  | otherwise = aStarSearch destination next heuristic cost queue exploredList
    where pop_branch = last branches -- check in the front (lowest heuristic value)
          priority_sort a b -- branches with heads having lower heuristic values go in the front of the queue (GT)
            | heuristic (head a) + cost a < heuristic (head b) + cost b = GT
            | otherwise = LT
          queue =  sortBy priority_sort (next pop_branch ++ init branches) -- always sort queue by priority
-- NOTE: `exploredList` is redundant since the `next` function already checks for repeated nodes.
-- NOTE: I thought `exploredList` could be a list of explored branches, but then again it didn't make sense either.

-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length branch

-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.

-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game
 | checkWin game 1 = 1 -- maxPlayer (human) won
 | checkWin game 0 = -1 -- minPlayer (computer) won
 | otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player -- implemented following the schema on page 164 and the pseudocode on page 166
 | terminal game = eval game -- terminal state
 | otherwise = if maxPlayer player
                then getMax (moves game player) (-1) player -- go through all actions and choose the best one (highest score)
                else getMin (moves game player) 1 player -- go through all actions and choose the best one (lowest score)
  where getMax availMoves bestScore player =
           if (null availMoves)
              then bestScore
                 else if ((minimax (head availMoves) (switch(player))) > bestScore)
                    then getMax (tail availMoves) (minimax (head availMoves) (switch(player))) player
                    else getMax (tail availMoves) bestScore player
        getMin availMoves bestScore player =
           if (null availMoves)
              then bestScore
                 else if ((minimax (head availMoves) (switch(player))) < bestScore)
                    then getMin (tail availMoves) (minimax (head availMoves) (switch(player))) player
                    else getMin (tail availMoves) bestScore player

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

--This algorithm was implemented by precisely following the pseudocode on page 170
alphabeta:: Game->Player->Int
alphabeta game player
  | terminal game = eval game
  | otherwise = if maxPlayer player
                 then getMax_ab player game (-2) 2 -- go through all actions and choose the best one (highest score), keeping track of alpha and beta
                 else getMin_ab player game (-2) 2 -- go through all actions and choose the best one (lowest score), keeping track of alpha and beta

maxRecord :: Player -> [Game] -> Int -> Int -> Int -> Int
maxRecord p games a b v
   | null games = v
   | v' >= b = v'
   | otherwise = maxRecord p (tail games) a' b v'
     where v' = max v (getMin_ab (switch p) (head games) a b)
           a' = max a v'

minRecord :: Player -> [Game] -> Int -> Int -> Int -> Int
minRecord p games a b v
   | null games = v
   | v' <= a = v'
   | otherwise = minRecord p (tail games) a b' v'
     where v' = min v (getMax_ab (switch p) (head games) a b)
           b' = min b v'

getMin_ab :: Player -> Game -> Int -> Int -> Int
getMin_ab p game a b
   | terminal game = eval game
   | otherwise = minRecord p (moves game p) a b 2

getMax_ab :: Player -> Game -> Int -> Int -> Int
getMax_ab p game a b
   | terminal game = eval game
   | otherwise = maxRecord p (moves game p) a b (-2)

--Testing function
testTicTacToe:: Game -> Player -> Bool
testTicTacToe g p = minimax g p == alphabeta g p

-- | Section 5.2 Wild Tic Tac Toe

-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game
  | checkWin game 1 || checkWin game 0 = 1
  | otherwise = 0

-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

--this is very similar to the normal alphabeta, with essentially only the evaluations differing
alphabetaWild:: Game->Player->Int
alphabetaWild game player
  | terminal game = if maxPlayer player
                    then (-1) * evalWild game -- reward times evaluation
                    else evalWild game -- reward times evaluation
  | otherwise = if maxPlayer player
                 then getMax_abW player game (-2) 2 -- go through all actions and choose the best one (highest score), keeping track of alpha and beta
                 else getMin_abW player game (-2) 2 -- go through all actions and choose the best one (lowest score), keeping track of alpha and beta

maxRecordW :: Player -> [Game] -> Int -> Int -> Int -> Int
maxRecordW p games a b v
  | null games = v
  | v' >= b = v'
  | otherwise = maxRecordW p (tail games) a' b v'
    where v' = max v (getMin_abW (switch p) (head games) a b)
          a' = max a v'

minRecordW :: Player -> [Game] -> Int -> Int -> Int -> Int
minRecordW p games a b v
  | null games = v
  | v' <= a = v'
  | otherwise = minRecordW p (tail games) a b' v'
    where v' = min v (getMax_abW (switch p) (head games) a b)
          b' = min b v'

getMin_abW :: Player -> Game -> Int -> Int -> Int
getMin_abW p game a b
  | terminal game = evalWild game
  | otherwise = minRecordW p (movesWild game p) a b 2

getMax_abW :: Player -> Game -> Int -> Int -> Int
getMax_abW p game a b
  | terminal game = (-1) * evalWild game
  | otherwise = maxRecordW p (movesWild game p) a b (-2)

-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined



			-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores

-- This function checks whether the given node (n) is viable as a continuation of the given existing branch (branch)
check::Node -> Branch -> Branch
check n [] = []
check n branch
  | not (elem n badNodesList) && not (elem n branch) && onthegrid n = n : branch
  | otherwise = []

-- This function checks whether the given node is on the grid
onthegrid::Node -> Bool
onthegrid (x, y)
  | x<1 = False
  | x>6 = False
  | y<1 = False
  | y>6 = False
  | otherwise = True
