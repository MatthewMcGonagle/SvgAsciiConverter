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

  hClose handle
