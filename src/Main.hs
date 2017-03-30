module Main where

import SvgAsciiConverter
import System.IO
import Control.Monad

main :: IO ()
main = do
  handle <- openFile "example.svg" ReadMode
  contents <- hGetContents handle
  let xmltags = parseXml contents
  putStrLn "The Xml tags are"
  print xmltags
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
      asciirow = take width' $ repeat ' '
      (Right (AsciiPicture painting')) = foldl (\acc x -> drawrectangle x (Painter '*') acc) canvas' `fmap` newrectlist 
  mapM_ putStrLn painting'
  
  hClose handle
