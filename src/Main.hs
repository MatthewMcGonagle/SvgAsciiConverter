module Main where

import SvgAsciiConverter
import System.IO

main :: IO ()
main = do
  handle <- openFile "example.svg" ReadMode
  contents <- hGetContents handle
  let svgelems = parseSvg contents
  putStrLn "svgelems = "
  print svgelems
  let rectlist = drop 1 `fmap` svgelems
      coord = head `fmap` svgelems
  putStrLn "After dropping first element, rectlist= "
  print rectlist 
  let teststring = take 10 $ repeat '-'
      paintedstring = drawsegmentrow 2 6 (Painter '*') teststring
      start = Coordinate 1 2
      dim = Dimensions 3 4
      myrect = Rectangle start dim
      canvas = AsciiPicture (take 10 $ repeat teststring)
      (AsciiPicture painting) = drawrectangle myrect (Painter '*') canvas
  putStrLn "The test string"
  putStrLn teststring
  putStrLn "The painted row"
  putStrLn paintedstring 
  putStrLn "The painting"
  mapM_ putStrLn painting

  hClose handle
