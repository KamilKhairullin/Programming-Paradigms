-- task 1
-- ([a] -> [a]) -> a -> [a] -> [a]

-- task 2
repeatUntil :: (a -> Bool) -> (a -> IO a) -> a -> IO a
repeatUntil p f x 
  | p x == False = f x  
  | otherwise = x 

-- task 3
type Point = (Double, Double) 
data Event = Click | Move Point

toPolyline :: [Event] -> [Point] 
toPolyline [] = [] 
toPolyline (x: xs: xss)
  | xs == Click = x : (toPolyline xss)
  | otherwise = toPolyline(xs:xss)
-- task 5

prefixes :: [a] -> [[a]]
prefixes (x:xs) = [] : (map (x:) (prefixes xs))
prefixes [] = [[]]