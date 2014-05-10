import CellularAutomata

--Creates a start line for the cellular automaton - a row of zeroes with a 1 at startIndex
seedLine :: Integer -> Integer -> [Integer]
seedLine 0 startIndex = []
seedLine 1 startIndex = if startIndex == 0 then [1] else [0]
seedLine n startIndex = (if startIndex == n then 1 else 0):seedLine (n - 1) startIndex

outputToPrintFromList :: [Integer] -> String
outputToPrintFromList [] = ""
outputToPrintFromList n = concat (map show n) ++ "\n"


generateOutputForAutomaton rule line iterations 
	| iterations == 0 = outputToPrintFromList nextLine
	| iterations > 0 = (outputToPrintFromList nextLine) ++ generateOutputForAutomaton rule nextLine (iterations-1)
	where nextLine = nextLineForLine line rule


main = do 
	let seed = seedLine 80 40
	let rule = 30
	putStrLn (outputToPrintFromList seed)
	putStrLn (generateOutputForAutomaton rule seed 50)
	
	
	