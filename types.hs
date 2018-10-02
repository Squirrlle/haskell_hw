x :: Int
x = 2

fac n = product [1..n]

my_pi :: Float
my_pi = 3.14

t :: Bool
t = True

xs :: [Int]
xs = [1,3,5,6]

xss :: [[Int]]
xss = [[2,4],[5,7]]

my_length :: [a] -> Int
my_length = sum [1 | _ <- xs]