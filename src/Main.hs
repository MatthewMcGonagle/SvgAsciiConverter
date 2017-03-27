module Main where

import SvgAsciiConverter
import System.IO
import Control.Monad

main :: IO ()
main = do
  handle <- openFile "example.svg" ReadMode
  contents <- hGetContents handle
  let svgelems = parseSvg contents
  putStrLn "svgelems = "
  print svgelems
  let rectlist = drop 1 `fmap` svgelems
      vbox = head `fmap` svgelems
  putStrLn "After dropping first element, rectlist= "
  print rectlist 
  putStrLn "The viewbox = "
  print vbox 
  let coord = viewboxtorect `fmap` vbox
      coord' = Rectangle (Coordinate 0 0) (Dimensions height' width')
      height' = 40
      width' = 80
  putStrLn "After converting from viewbox to rectangle, we have"
  print coord 
  let newrectlist = rectlist >>= mapM (\x -> rectchange <$> coord <*> Right coord' <*> conversionmap x) 
      conversionmap = Right . svgrecttorect 
  putStrLn "After coordinate conversion, the rectange list = "
  print newrectlist
  let canvas' = AsciiPicture asciiarray
      asciiarray = take height' $ repeat asciirow
      asciirow = take width' $ repeat '-'
      (Right (AsciiPicture painting')) = foldl (\acc x -> drawrectangle x (Painter '*') acc) canvas' `fmap` newrectlist 
  mapM_ putStrLn painting'
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
