import CodeWorld

data Line a = Line [a] a [a] deriving (Show)

integers :: Line Integer
integers = Line [-1, -2, -3, -4, -5] 0 [1, 2, 3, 4, 5]

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
genLine f x g = Line (genList f (f x) x) x (genList g (g x) x)

-- | Generate a list by using generating function.
-- (genList f (f x) x) generates a list by applying f to x until reaching Nothing to produce 
genList :: (a -> Maybe a) -> Maybe a -> a -> [a]
genList f (Just result) x = result : (genList f (f x) x)
genList f Nothing x = [] 

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

main :: IO()
main = do
  --print(integers)
  --print(cutLine 3 integers)
  --print(mapLine (^2) integers)
  --print(zipLines integers integers)
  print(zipLinesWith (*) integers integers)
  print("Hello")