module Main where

import SvgAsciiConverter

main :: IO ()
main = do
  let teststring = take 10 $ repeat '-'
      paintedstring = drawsegmentrow 2 6 (Painter '*') teststring
      start = Coordinate 1 2
      finish = Coordinate 4 5
      canvas = AsciiPicture (take 10 $ repeat teststring)
      (AsciiPicture painting) = drawrectangle start finish (Painter '*') canvas
  putStrLn "The test string"
  putStrLn teststring
  putStrLn "The painted row"
  putStrLn paintedstring 
  putStrLn "The painting"
  mapM_ putStrLn painting
