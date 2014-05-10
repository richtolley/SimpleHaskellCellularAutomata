import CellularAutomata

--Creates a start line for the cellular automaton - a row of zeroes with a 1 at startIndex
seedLine :: Integer -> Integer -> [Integer]
seedLine 0 startIndex = []
seedLine 1 startIndex = if startIndex == 0 then [1] else [0]
seedLine n startIndex = (if startIndex == n then 1 else 0):seedLine (n - 1) startIndex

--Takes an array representing the state of an automaton and converts it to a string
automatonLineToString :: [Integer] -> String
automatonLineToString [] = ""
automatonLineToString n = concat (map show n) ++ "\n"

--Outer function for running an automaton. Concatenates the seed line with 
--the output of the rest of the automaton
runAutomaton :: Integer -> [Integer] -> Integer -> String
runAutomaton rule seed iterations
	| iterations > 0 = (automatonLineToString seed) ++ runAutomatonHelper rule seed (iterations-1)
	| otherwise = ""	

--Recursively assembles the output for the given automaton line by line
runAutomatonHelper ::  Integer -> [Integer] -> Integer -> String
runAutomatonHelper rule line iterations 
	| iterations == 0 = automatonLineToString nextLine
	| iterations > 0 = (automatonLineToString nextLine) ++ runAutomaton rule nextLine (iterations-1)
	where nextLine = nextLineForLine line rule


main = do 
	let seed = seedLine 80 40
	putStrLn "Please specify the Wolfram number of the automaton you wish to print"
	ruleStr <- getLine
	let rule = read ruleStr :: Integer
	putStrLn (runAutomaton rule seed 50)
	
	
	