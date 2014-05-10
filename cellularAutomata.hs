module CellularAutomata where

import Data.Map(Map)
import Data.Bits

--For any list, if the list length is greater than 0, return a list containing the last element,
--followed by the whole list, followed by the first element. To allow wrapping of cell neighbourhoods
--in a one dimensional cellular automaton
wrappedLine :: [Integer] -> [Integer]
wrappedLine [] = []
wrappedLine n = (last n:n) ++ [n !! 0]

neighborhoodsForLine :: [Integer] -> [[Integer]]
neighborhoodsForLine n
					| length n < 3 = []
					| otherwise    = [(take 3 n)] ++ neighborhoodsForLine(drop 1 n)
						     
--Takes the bit field of a neighborhood as a list and returns the binary number it represents
--e.g [1,1,1] -> 7
stateForNeighborhood :: [Integer] -> Integer
stateForNeighborhood [] = 0
stateForNeighborhood (x:xs) = (if x /= 0 then setBit 0 len else 0) + stateForNeighborhood xs
	where len = length xs

--Takes a list of neighborhoods and returns a list containing the binary value of each neighborhood
statesForNeighborhoods :: [[Integer]] -> [Integer]
statesForNeighborhoods [] = []
statesForNeighborhoods (x:xs) = stateForNeighborhood x : statesForNeighborhoods xs

--Takes a rule and a state and returns the next state
nextStateForRule :: Integer -> Integer -> Integer
nextStateForRule rule currState = if testBit rule (fromIntegral currState) then 1 else 0

--Maps a set of states to the next set of states using a rule
nextLineForStates :: [Integer] -> Integer -> [Integer]
nextLineForStates [] rule = []
nextLineForStates (x:xs) rule = nextStateForRule rule x : nextLineForStates xs rule
 
--Takes a line of a 1D cellular automaton and returns the next line
nextLineForLine :: [Integer] -> Integer -> [Integer]
nextLineForLine [] rule = []
nextLineForLine n rule = nextLineForStates (statesForNeighborhoods (neighborhoodsForLine (wrappedLine n))) rule






