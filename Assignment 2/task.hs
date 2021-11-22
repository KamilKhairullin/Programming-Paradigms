import CodeWorld

data Line a = Line [a] a [a] deriving (Show)

integers :: Line Integer
integers = Line [-1, -2] 0 [1, 2]

forceUnwrap :: Maybe a -> a
forceUnwrap (Just a) = a

-- | Keep up to a given number of elements in each direction in a line. 
-- cutLine 3 integers = Line [-1,-2,-3] 0 [1,2,3]
cutLine :: Int -> Line a -> Line a
cutLine n (Line a b c) = Line (take n a) b (take n c)


-- | Generate a line by using generating functions.
-- (genLine f x g) generates a line with x in its focus,
-- then it applies f to x until reaching Nothing to produce -- a list of elements to the left of x,
-- and, similarly, applies g to x until reaching Nothing to -- produce a list of elements to the right of x.
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (genList f x) x (genList g x)

-- | Generate a list by using generating function.
-- (genList f x) generates a list by applying f to x until reaching Nothing to produce 
genList :: (a -> Maybe a) -> a -> [a]
genList f x = 
  case f x of
    Nothing   -> []
    Just result  -> result : (genList f result)
    
    
-- | Apply a function to all elements on a line.
-- mapLine (^2) integers = Line [1, 4, 9, ..] 0 [1, 4, 9, ..] 
mapLine :: (a -> b) -> Line a -> Line b 
mapLine f (Line a b c) = Line (map f a) (f b) (map f c)

-- | Zip together two lines.
-- zipLines integers integers
-- = Line [(-1,-1),(-2,-2),..] (0,0) [(1,1),(2,2),..] 
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line a b c) (Line d e f) = Line (zip a d) (b, e) (zip c f)

-- | Zip together two lines with a given combining function. 
-- zipLinesWith (*) integers integers
-- = Line [1,4,9,..] 0 [1,4,9,..]
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith func (Line a b c) (Line d e f) = Line (zipWith func a d) (func b e) (zipWith func c f)

data Cell = Alive | Dead deriving (Show)
  
  
states :: Cell -> Cell -> Cell -> Cell
states Dead Dead Dead = Dead
states Dead Dead Alive = Alive
states Dead Alive Dead = Alive
states Dead Alive Alive = Alive
states Alive Dead Dead = Alive
states Alive Dead Alive = Dead
states Alive Alive Dead = Dead
states Alive Alive Alive = Dead

rule30 :: Line Cell -> Cell
rule30 (Line [] x []) = states Dead x Dead
rule30 (Line [] x (y:ys)) = states Dead x y
rule30 (Line (y:ys) x []) = states y x Dead
rule30 (Line (y:ys) x (z:zs)) = states y x z

-- | Applies rule30 to each cell of line
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- | Shfis the focus to one position left
-- shiftLeft integers = Just (Line [-2,-3,-4,-5,..] (-1) [0,1,2,3,4,5,..])
shiftLeft  :: Line a -> Maybe (Line a)
shiftLeft (Line (x:xs) b c) = Just (Line xs x (b:c)) 
shiftLeft (Line [] _ _) = Nothing

-- | Shfis the focus to one position right
--shiftRight integers = Just (Line [0,-1,-2,-3,-4,-5] 1 [2,3,4,5])
shiftRight  :: Line a -> Maybe (Line a)
shiftRight (Line b c (x:xs)) = Just (Line (c:b) x xs)
shiftRight (Line _ _ []) = Nothing


-- | An entire line of different focus shifts for given Line
-- lineShifts integers = Line [Line [] (-2) [-1,0,1,2],Line [-2] (-1) [0,1,2]] (Line [-1,-2] 0 [1,2]) [Line [0,-1,-2] 1 [2],Line [1,0,-1,-2] 2 []]
lineShifts :: Line a -> Line (Line a)
lineShifts line = Line leftShift line rightShift
  where
    leftShift = reverse (genList shiftLeft line)
    rightShift = genList shiftRight line

-- | Render a line of 1x1 pictures.
renderLine :: Line Picture -> Picture
-- | Render the fist N steps of Rule 30,
-- applied to a given starting line. renderRule30 :: Int -> Line Cell -> Picture
    
main :: IO()
main = do
  --print(integers)
  --print(cutLine 3 integers)
  --print(mapLine (^2) integers)
  --print(zipLines integers integers)
  --print(shiftRight integers)
  --print(zipLinesWith (*) integers integers)
  print(lineShifts integers)
  print("Hello")