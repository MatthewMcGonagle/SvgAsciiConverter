module Main where

import SvgAsciiConverter
import System.IO
import Control.Monad
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  handle <- openFile "example.svg" ReadMode
  contents <- hGetContents handle

  let eitherXml = parseXml contents
  case eitherXml of Left parseError -> error (show parseError) 
                    otherwise -> return ()

  let (Right xml) = eitherXml
      xmlSvg = head xml
      (XmlTag name ps subs) = xmlSvg
      pMap = Map.fromList ps
      maybeVBoxPString = Map.lookup "viewBox" pMap
  case maybeVBoxPString of Nothing -> error "viewBox parameter not found"
                           otherwise -> return ()

  let (Just vBoxString) = maybeVBoxPString
      eitherVBox = parseVBoxParameters vBoxString 

  case eitherVBox of Left parseError -> error (show parseError)
                     otherwise -> return ()
  let (Right vBox) = eitherVBox
      subs' = filter (\ (XmlTag n _ _) -> n `elem` ["rect"]) 
                     subs
      xmlSvg' = XmlTag name ps subs'
      interestingP (key, value) = key `elem` ["x", "y", "height", "width", "style"] 
      xmlFiltered = (filter interestingP) `fmap` xmlSvg'
      xmlDict = Map.fromList `fmap` xmlFiltered 
      
  let (XmlTag _ _ subs) = xmlDict
      interestingStyleKeys = ["fill"]
      maybeRects = map (\ (XmlTag _ ps _) -> mapInterestingToRectangle interestingStyleKeys ps) 
                   subs 
      justsFilter (Just x) = True
      justsFilter Nothing = False
      filteredRects = filter justsFilter maybeRects 
      rects = map (\ (Just x) -> x) filteredRects 

  let coord' = Rectangle (Coordinate 0 0) (Dimensions height' width') ()
      height' = 60
      width' = 100
      rectMap x = rectFloatToInt vBox coord' x
      rectInts = map rectMap rects 

  putStrLn "After coordinate conversion, the rectange list = "
  print rectInts 

  let canvas' = AsciiPicture asciiarray
      asciiarray = take height' $ repeat asciirow
      asciirow = take width' $ repeat ' '
      pictureMap = foldl (\acc x -> drawRectInt x (Painter '*') acc) canvas'
      (AsciiPicture painting') = pictureMap rectInts 
  mapM_ putStrLn painting'

  hClose handle
