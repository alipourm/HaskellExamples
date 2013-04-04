-- @author Mohammad Amin Alipour 
-- @email  aminalipour@gmail.com
-- @args a list of integers
-- @returns a list that contains the difference between 
-- consecutive elements of the list
                   
diff :: [Int] -> [Int]
diff [] = [] 
diff (fst:snd:tl) = (snd-fst):diff (snd:tl)
diff _ = []

test1 = diff [4,2,3,1,5,7] -- should return [-2, 1,-2,4,2]
test2 = diff [4]           -- should return []
test3 = diff [1,4]         -- should return [3]