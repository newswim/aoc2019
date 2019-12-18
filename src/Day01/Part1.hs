module Day01 where

findFuel :: Integer -> Integer
findFuel mass = (div mass 3) - 2

findAllFuel :: Integer -> Integer
findAllFuel mass
 | s <= 0 = 0
 | otherwise = findAllFuel s + s
 where s = findFuel mass

part01 :: [Integer] -> [Integer]
part01 input = map findFuel input

part02 :: [Integer] -> [Integer]
part02 = map findAllFuel

main :: IO ()
main = do
  inputs <- lines <$> readFile "src/Day01/input"
  let moduleMasses = read <$> inputs
  putStrLn $ show $ sum $ part01 moduleMasses
  putStrLn $ show $ sum $ part02 moduleMasses
