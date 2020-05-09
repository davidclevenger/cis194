{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle, midCircle:: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))
midCircle c = colored c (translated 0   0.0  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Int -> Picture
trafficLight 0 = botCircle green & midCircle black & topCircle black & frame
trafficLight 1 = botCircle black & midCircle yellow & topCircle black  & frame
trafficLight 2 = botCircle black & midCircle black & topCircle red  & frame


trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 5 == 0 = trafficLight 0
  | round (t/3) `mod` 5 == 1 = trafficLight 0
  | round (t/3) `mod` 5 == 2 = trafficLight 1
  | round (t/3) `mod` 5 == 3 = trafficLight 2
  | round (t/3) `mod` 5 == 4 = trafficLight 2

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

limit = 8
tree :: Integer -> Picture
tree n
  | n == 0 = blank
  | n == 1 = path [(0,0),(-1,1)]
             & path [(0,0),(1,1)] 
             & path [(-1,1),(0,2)] 
             & path [(1,1),(0,2)]
  | n < limit = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))
  | n >= limit = tree limit

 
treeController :: Double -> Picture
treeController t = tree (round(t))

  
exercise2 :: IO ()
exercise2 = animationOf treeController

-- Exercise 3

wall, ground, storage, box :: Picture
wall = colored grey (solidRectangle 2.5 2.5)
ground =  colored yellow (solidRectangle 2.5 2.5)
storage = colored brown (solidRectangle 2.5 2.5)
box = colored yellow (solidRectangle 2.5 2.5) & colored black (solidCircle 1)

-- map tile type to picture
drawTile :: Integer -> Picture
drawTile c =
  | c == 0 = wall
  | c == 1 = ground
  | c == 3 = box

-- map time (unused) to picture
pictureOfMaze :: Double -> Picture
pictureOfMaze _ = drawTile 10

exercise3 :: IO ()
exercise3 = animationOf pictureOfMaze
   
-- map coordinate to tile type
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
