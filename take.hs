myTake:: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x:xs)
	|n <= 0		= []
	|otherwise	= x : myTake (n-1) xs
	
type Cell = Int
type Board = [Cell]
type Roll = Int

{-
1. Number of steps so far
2. Rolls I have left
3. Board that I have left
4. Entire Board
-}
chutesLadders :: Int -> [Roll] -> Board -> Board -> Int
chutesLadders n _ [] _ = n
chutesLadders count (roll:rolls) remaining entire
	|null newBoard		= n
	|newCell == 0		= chutesLadders (count+1) rolls newBoard entire
	|otherwise			= chutesLadders (count+1) rolls (drop newCell entire) entire
		where newBoard = drop roll remaining
				newCell = head newBoard
			
board :: Board
board = [0,0,5,2,0,8,3,0,0,0,0]

rolls :: [Rolls]
rolls = (cycle [1,2,3])