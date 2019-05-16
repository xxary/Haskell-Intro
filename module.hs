t0 :: (Int,Int)
t0 = (1234,5678)

t1 ::(String,Int,Double)
t1 = ("sometext",8,3.141)

t2 :: ([Int],[String],(Float,Char))
t2 = ([1,2,3,4],["aaa","bbb"],(1.0,'o'))

main :: IO()
main = do
	print t0
	print t1
	print t2
	

------------------------------------------
f0 :: String -> Int
f0 = length

f1 :: String -> (Stromg,Int)
f1 x = (x, length x)

f2 :: [String] -> [(String,Int)]
f2 = map f1

main :: IO()
main = do
	print $ f0 "hello"
	print $ f1 "hello"
	print $ f2 ["hello","goodbye"]
	
---------------------------------------------
import Data.List

formatList :: String -> (String -> (String -> [(String] -> String)))
formatList start end separator xs = start ++ (intercalate separator (map show xs)) ++end

main :: IO()
main = putStrLn $ formatList "<list>" "</list>" "|" ["first","second","third","fourth"]

--------------------------------------------
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f(a : as) = f a : myMap f as

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f(a : as) = if f a then a : myFilter f as else myFilter f as

myFold :: (a -> b -> b) -> b -> [a] ->b
myFold _ b [] = b
myFold f b (a : as) = myFold f (f a b) as

main :: IO()
main = do
	print $ myMap show [10,20,30]
	print $ myFilter ( < 25) [10,20,30]
	print $ myFold (+) 100 [10,20,30]
	
------------------------------------------------------
module ColorRGB(color) where

data Color = RGB
	{ red :: Int
	, green :: Int
	, blue :: Int
	} deriving Show
	
	>x = RGB 10 20 30 
	>x
	>red x
	
module ColorRGBCMYK(Color) where

data Color = RGB Int Int Int | CMYK Float Float Float Float deriving show

	>:info Color
	>:info RGB
	>x = RGB 11 22 33 
	>:type x
	>x
