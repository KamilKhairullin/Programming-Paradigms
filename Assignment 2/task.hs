import CodeWorld

data Line a = Line [a] a [a] deriving (Show)
data Side = Left | Right

integers :: Line Integer
integers = Line [-1, -2] 0 [1, 2]

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
    leftShift = genList shiftLeft line
    rightShift = genList shiftRight line

        
data Cell = Alive | Dead deriving (Show)
  
-- | All states of rule30 
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

-- | Render a line of 1x1 pictures.
renderLine :: Line Picture -> Picture
renderLine (Line a b c) = left <> b <> right
  where
    left = renderArray a Main.Left
    right = renderArray c Main.Right

-- | Renders array of Pictures to single Picture
renderArray :: [Picture] -> Side -> Picture
renderArray (x:xs) Main.Left = translated (-1) 0  (x <> renderArray xs Main.Left)
renderArray (x:xs) Main.Right = translated 1 0 (x <> renderArray xs Main.Right)
renderArray [] _ = blank

-- | Translates cell to Picture
-- Alive is black
-- Dead is grey
cellToPicture :: Cell -> Picture
cellToPicture Alive = (colored black (solidRectangle 1 1))
cellToPicture Dead = (colored grey (solidRectangle 1 1))

-- | Translates line of cells to line of Pictures
cellLineToPicture :: Line Cell -> Line Picture
cellLineToPicture line = mapLine (\x -> cellToPicture x) line


-- | Render the fist N steps of Rule 30,
-- applied to a given starting line. 
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 n line
  | n > 0 = currentLinePicture <> translated 0 (-1) (renderRule30 (n - 1) newLine)
  | otherwise = renderLine (cellLineToPicture line)
  where
    currentLinePicture = renderLine (cellLineToPicture line)
    newLine = applyRule30 line

-- | Applies rule30 to each cell of line
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

startState :: Int -> Line Cell
startState n = Line (replicate n Dead) Alive (replicate n Dead)

-- 1.3 Discrete spaces

-- | A descrete 2D space with a focus.
-- A 2D space is merely a (vertical) line
-- where each element is a (horizontal) line. 
data Space a = Space (Line (Line a)) deriving (Show)

productOfLines :: Line a -> Line b -> Space (a, b)
productOfLines (Line l1 f1 r1) (Line l2 f2 r2) = Space (Line left focus right)
  where
    left = map (\l -> Line (l2 >>= \e -> [(l, e)]) (l, f2) (r2 >>= \e -> [(l, e)])) l1
    focus = Line (l2 >>= \e -> [(f1, e)]) (f1, f2) (r2 >>= \e -> [(f1, e)])
    right = map (\r -> Line (l2 >>= \e -> [(r, e)]) (r, f2) (r2 >>= \e -> [(r, e)])) r1

main :: IO()
main = --drawingOf (renderRule30 500 (startState 500) )
  do
    --print(test integers integers)
--print(applyRule30 (Line (deadCellsList 10) Alive (deadCellsList 10)))
  --print(integers)
  --print(cutLine 3 integers)
  --print(mapLine (^2) integers)
  --print(zipLines integers integers)
  --print(shiftRight integers)
  --print(zipLinesWith (*) integers integers)
--  print(lineShifts integers)
  --print(productOfLines2 integers integers)
    print(productOfLines integers integers)
--  print("Hello")