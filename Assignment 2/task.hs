import CodeWorld

-- 1.1 Discrete lines and spaces -----------------------------------------------

-- | A line with a focus.
-- Line xs y zs represents a descrete line:
-- * xs represents all elements to the left (below) -- * y is the element in focus
-- * zs represents all elements after (above)
data Line a = Line [a] a [a] deriving (Show)

-- | A side with to possible values. 
-- Used to properly rotate mirrored images
data Side = Left | Right

integers :: Line Integer
integers = Line [-1, -2, -3, -4, -5] 0 [1, 2, 3, 4, 5]

integers2 :: Line Integer
integers2 = Line [-10, -9] (-8) [-7, -6]

-- | Keep up to a given number of elements in each direction in a line. 
-- cutLine 3 integers = Line [-1,-2,-3] 0 [1,2,3]
cutLine :: Int -> Line a -> Line a
cutLine n (Line l f r) = Line (take n l) f (take n r)


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

-- 1.2 Rule 30 -----------------------------------------------------------------

-- | Cell of Rule 30 game, which is either Alive or Dead
data Cell = Alive | Dead deriving (Show)

-- | Returns next state for Cell in focus of line of cells
rule30 :: Line Cell -> Cell
rule30 (Line [] x []) = states Dead x Dead
rule30 (Line [] x (y:ys)) = states Dead x y
rule30 (Line (y:ys) x []) = states y x Dead
rule30 (Line (y:ys) x (z:zs)) = states y x z
  
-- | All states of Rule30 game 
states :: Cell -> Cell -> Cell -> Cell
states Dead Dead Dead = Dead
states Dead Dead Alive = Alive
states Dead Alive Dead = Alive
states Dead Alive Alive = Alive
states Alive Dead Dead = Alive
states Alive Dead Alive = Dead
states Alive Alive Dead = Dead
states Alive Alive Alive = Dead

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


-- | Applies rule30 to each cell of line
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)


-- | Render a line of 1x1 pictures.
renderLine :: Line Picture -> Picture
renderLine (Line a b c) = left <> b <> right
  where
    left = renderArray a Main.Left
    right = renderArray c Main.Right

-- | Render the fist N steps of Rule 30,
-- applied to a given starting line. 
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 n line
  | n > 0 = currentLinePicture <> translated 0 (-1) (renderRule30 (n - 1) newLine)
  | otherwise = renderLine (cellLineToPicture line)
  where
    currentLinePicture = renderLine (cellLineToPicture line)
    newLine = applyRule30 line


-- | Renders array of Pictures to single Picture
renderArray :: [Picture] -> Side -> Picture
renderArray (x:xs) Main.Left = translated (-1) 0  (x <> renderArray xs Main.Left)
renderArray (x:xs) Main.Right = translated 1 0 (x <> renderArray xs Main.Right)
renderArray [] _ = blank

-- | Translates cell to Picture
-- Alive is black
-- Dead is grey
cellToPicture :: Cell -> Picture
cellToPicture Alive = (colored green (solidRectangle 1 1))
cellToPicture Dead = (colored black (solidRectangle 1 1))

-- | Translates line of cells to line of Pictures
cellLineToPicture :: Line Cell -> Line Picture
cellLineToPicture line = mapLine (\x -> cellToPicture x) line

-- 1.3 Discrete spaces ---------------------------------------------------------

-- | A descrete 2D space with a focus.
-- A 2D space is merely a (vertical) line
-- where each element is a (horizontal) line. 
data Space a = Space (Line (Line a)) deriving (Show)

-- | Returns a cartesian product of two lines as a 2D Space
productOfLines :: Line a -> Line b -> Space (a, b)
productOfLines (Line l1 f1 r1) (Line l2 f2 r2) = Space (Line left focus right)
  where
    left = map (\l -> Line (l2 >>= \e -> [(l, e)]) (l, f2) (r2 >>= \e -> [(l, e)])) l1
    focus = Line (l2 >>= \e -> [(f1, e)]) (f1, f2) (r2 >>= \e -> [(f1, e)])
    right = map (\r -> Line (l2 >>= \e -> [(r, e)]) (r, f2) (r2 >>= \e -> [(r, e)])) r1

-- | Maps given function to every element of space
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace func (Space (Line l f r)) = Space (Line newL newF newR)
  where
    newL = map (mapLine func) l
    newF = mapLine func f
    newR = map (mapLine func) r
-- | Zip together two spaces as (a, b)
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line l1 f1 r1)) (Space (Line l2 f2 r2)) = (Space (Line newL newF newR))
  where 
    newL = zipWith zipLines l1 l2
    newF = zipLines f1 f2
    newR = zipWith zipLines r1 r2

-- | Zip together two spaces with given function.
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith func (Space (Line l1 f1 r1)) (Space (Line l2 f2 r2)) = (Space (Line newL newF newR))
  where 
    newL = zipWith (zipLinesWith func) l1 l2
    newF = zipLinesWith func f1 f2
    newR = zipWith (zipLinesWith func) r1 r2

-- 1.4 Game of Life ------------------------------------------------------------

conwayRule :: Space Cell -> Cell
conwayRule space = nextState cell (countAliveAround space)
  where
    Space(Line _ (Line _ cell _) _) = space
    
    nextState :: Cell -> Int -> Cell
    nextState Dead 3 = Alive
    nextState Dead _ = Dead
    nextState Alive 2 = Alive
    nextState Alive 3 = Alive
    nextState Alive _ = Dead
        
    isAlive :: Cell -> Int
    isAlive Alive = 1
    isAlive Dead = 0

    numberOfNeighborsOnLine :: Line Cell -> Int
    numberOfNeighborsOnLine (Line [] f []) =  isAlive f 
    numberOfNeighborsOnLine (Line [] f (n:_)) = isAlive f + isAlive n
    numberOfNeighborsOnLine (Line (p:_) f []) = isAlive f + isAlive p
    numberOfNeighborsOnLine (Line (p:_) f (n:_)) = isAlive p + isAlive f + isAlive n
    
    numberOfNeighborsOnFocusLine :: Line Cell -> Int
    numberOfNeighborsOnFocusLine (Line [] f []) =  0
    numberOfNeighborsOnFocusLine (Line [] f (n:_)) = isAlive n
    numberOfNeighborsOnFocusLine (Line (p:_) f []) = isAlive p
    numberOfNeighborsOnFocusLine (Line (p:_) f (n:_)) = isAlive p + isAlive n    

    countAliveAround :: Space Cell -> Int
    countAliveAround (Space (Line [] f [])) = numberOfNeighborsOnFocusLine f 
    countAliveAround (Space (Line (top:_) f [])) = numberOfNeighborsOnLine top + numberOfNeighborsOnFocusLine f 
    countAliveAround (Space (Line [] f (bot:_))) = numberOfNeighborsOnLine bot + numberOfNeighborsOnFocusLine f 
    countAliveAround (Space (Line (top:_) f (bot:_))) =  numberOfNeighborsOnLine bot + numberOfNeighborsOnFocusLine f + numberOfNeighborsOnLine top



spaceShiftLeft  :: Space a -> Maybe (Space a)
spaceShiftLeft (Space (Line (x:xs) b c)) = Just (Space (Line xs x (b:c))) 
spaceShiftLeft (Space (Line [] _ _)) = Nothing

spaceShiftRight  :: Space a -> Maybe (Space a)
spaceShiftRight (Space (Line b c (x:xs))) = Just (Space (Line (c:b) x xs))
spaceShiftRight (Space (Line _ _ [])) = Nothing

spaceLineShift :: Space a -> Line (Space a)
spaceLineShift (Space (Line l f r)) = (Line left focus right)
  where
    leftShifts = genList shiftLeft f
    rightShifts = genList shiftRight f
    left = map (\shift -> Space(Line l shift r)) leftShifts
    focus = Space(Line l f r)
    right = map (\shift -> Space(Line l shift r)) rightShifts

spaceShifts :: Space a -> Space (Space a)
spaceShifts space = Space (Line l f r)
  where
    leftShift = genList spaceShiftLeft space
    rightShift = genList spaceShiftRight space
    l = map (\sp -> spaceLineShift sp) leftShift
    f = spaceLineShift space
    r = map (\sp -> spaceLineShift sp) rightShift
    
applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)


renderArrayOfLines :: [Line Picture] -> Picture
renderArrayOfLines [] = blank
renderArrayOfLines (x:xs) = (renderLine x) <> translated 0 (-1) (renderArrayOfLines xs)

renderSpace :: Space Picture -> Picture
renderSpace (Space (Line l f r)) = picture
  where 
    picture = renderArrayOfLines l <> translated 0 (-1) (renderLine f) <> renderArrayOfLines r

spaceCellToPicture :: Space Cell -> Space Picture
spaceCellToPicture space = mapSpace (\x -> cellToPicture x) space


getCurrentState :: Int -> Space Cell -> Space Cell 
getCurrentState time currentState
  | time <= 1 = currentState
  | otherwise = getCurrentState (time - 1) (applyConwayRule currentState)
  
-- | Animate Conway's Game of Life, 
-- starting with a given space
-- and updating it every second.
animateConway :: Space Cell -> IO ()
animateConway space = animationOf gameOfLife
  where 
    gameOfLife :: Double -> Picture 
    gameOfLife time = renderSpace(spaceCellToPicture(getCurrentState (floor time) space))

startConwayLine1 :: Line Cell
startConwayLine1 = Line (Alive : replicate 10 Dead) Alive (replicate 11 Dead)

startConwayLine2 :: Line Cell
startConwayLine2 = Line ((replicate 11 Dead)) Alive (Alive : replicate 11 Dead)


startConwayLine3 :: Line Cell
startConwayLine3 = Line (Alive : (replicate 10 Dead)) Alive (Alive : replicate 10 Dead)

startConwayLine4 :: Line Cell
startConwayLine4 = Line (replicate 11 Dead) Dead (replicate 11 Dead)



startConway :: Space Cell
startConway = (Space (Line (startConwayLine1 : (replicate 10 startConwayLine4)) startConwayLine2 (startConwayLine3 : (replicate 10 startConwayLine4))))


startState :: Int -> Line Cell
startState n = Line (replicate n Dead) Alive (replicate n Dead)

startRule30 :: IO()
startRule30 = drawingOf (renderRule30 100 (startState 100) )

main :: IO()
main = do
  -- task 1.1
  print("Task 1.1") 
  print(cutLine 3 integers)
  -- task 1.3 
  print("Task 1.3:")
  print(mapLine (^2) integers)
  -- task 1.4
  print("Task 1.4:")
  print(zipLines integers integers)
  print(zipLinesWith (*) integers integers)
  -- task 1.6
  print("Task 1.6")
  print(shiftRight integers)
  -- task 1.7
  print("Task 1.7")
  print(lineShifts integers)
  -- task 1.8
  --startRule30
  -- task 1.9
  print("Task 1.9")
  print(productOfLines integers integers)
  -- task 1.10
  print("Task 1.10. zipSpaces")
  print(zipSpaces (productOfLines integers integers) (productOfLines integers integers))
  
--animateConway startConway
--drawingOf (renderRule30 500 (startState 500) )
    --print(test integers integers)
--print(applyRule30 (Line (deadCellsList 10) Alive (deadCellsList 10)))
  --print(integers)
--    print("Hello")