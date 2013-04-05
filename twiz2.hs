-- @author mohammad amin alipour



-- Generic tables
--
type Row a = [a]
type Col a = [a]
type Table a = [Row a]

type X = Int
type Y = Int
type Pos = (X,Y)

-- Pictures
--
type Pixel = Bool
type Line  = Row Pixel  
type Pic   = Table Pixel

blank = False
pixel = True

black = '#'
white = ' '


t = [[True, True, True ],
     [False,True, False],
     [False,True, False]]

t1 = [[True, True, True ],
     [False,True, False],
     [False,True]]



-- map for a list of lists
mapT :: (a -> b) -> Table a -> Table b 
mapT f [] = []
mapT f (firstRow: restOfTable) =  (map f firstRow): mapT f restOfTable

test1 = mapT not t 
-- > [[False,False,False],[True,False,True],[True,False,True]]
test2 = mapT not (mapT not t) == t 
-- > True 

-- composing a new list from two lists, we assume the length of list are equal.
mergeRow :: (a -> b -> c) -> Row a -> Row b -> Row c
mergeRow f [] [] = []
mergeRow f (hd1:tl1) (hd2:tl2) = (f hd1 hd2): mergeRow f tl1 tl2
mergeRow _ _ _ = error "size of rows of arguments are not similar"


row1 = [1,2,3]
row2 = [2,3,4]
row3 = [5,6]
test3 = mergeRow (+) row1 row2 
-- > [3,5,7]  
test4 = mergeRow (-) row2 row1 
-- > [1,1,1]
test5 = mergeRow (*) row2 row3 
-- > [10,18*** Exception: size of rows of arguments are not similar

-- merging two tables, we assume dimensions of tables are the same
mergeTables :: (a -> b -> c) -> Table a -> Table b -> Table c
mergeTables f [] [] = [] --empty tables results in empty table
mergeTables f (r1:t1) (r2:t2) = mergeRow f r1 r2: mergeTables f t1 t2  
mergeTables _ _ _ = error "number of columnes are not the same"

test6 = mergeTables (||) t1 t1 
-- > [[True,True,True],[False,True,False],[False,True]]

test7 = mergeTables (&&) (mapT not t1) t1 
-- >[[False,False,False],[False,False,False],[False,False]]



-- overlaying binary pictures
overlay = (||)
union :: Pic -> Pic -> Pic
union  = mergeTables overlay

test8 = union (mapT not t1) t1 
--> [[True,True,True],[True,True,True],[True,True]]

-- multiple application of a function
times :: Int -> (a -> a) -> a -> a
times 1 f = f
times n f = f . times (n-1) f

test9 = times 10 (+ 1) 0 
-- > 10
test10 = times 10 (++ "A") "" 
-- > "AAAAAAAAAA"