compareReact h1 w1 h2 w2
	| h1 * w1 >= h2 * w2	=h1 * w1
	| otherwise 			=h2 * w2
	
compareReact2 h1 w1 h2 w2
	| area1 >= area2	=area1
	|otherwise			=area2
		where area1 = h1 * w1
			area2 = h2 * w2
			
volume h w = area
	where area = h * w
	
volume2 h w = let area h * w in area

grades :: [(String, [Int])]
grades = [("ann", [90,80,70]), ("bill", [70,50])]

avgGrade :: [(String, [Int])] -> Int
avgGrade gs = sum g1s `div` lenght g1s 
	where gls = [y | (_,y:_) <- gs]
	
ages :: [(String, Int)]
ages = [("ann", 51), "bill", 52), ("charlie" 48)]

minOver50 :: [(String,Int)] -> (Bool, Int)
minOver50
	where as = [y| (_,y) <- ages, y >= 50]