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
      maybeVBoxPString = Map.lookup "viewBox" (Map.fromList ps) 
  case maybeVBoxPString of Nothing -> error "viewBox parameter not found"
                           otherwise -> return ()

  let (Just vBoxString) = maybeVBoxPString
      eitherVBox = parseVBoxParameters vBoxString 

  case eitherVBox of Left parseError -> error (show parseError)
                     otherwise -> return ()

  let (Right vBox) = eitherVBox
      subIsInteresting (XmlTag n _ _) = n `elem` ["rect"]
      pIsInteresting (x, y) = x `elem` ["x", "y", "height", "width", "style"] 
      xmlFiltered = filterInterestingSubs pIsInteresting subIsInteresting xmlSvg
      xmlDict = Map.fromList `fmap` xmlFiltered 
      
      xmlDictToMRectangle (XmlTag _ dict _) = do
            rectangleKeys <- mapToMRectangleKeys dict
            let interestingStyle (k, v) = k `elem` ["fill"]
                rectangleInteresting = (filter interestingStyle) `fmap` rectangleKeys
                rectangleDict = Map.fromList `fmap` rectangleInteresting
            return rectangleDict
      maybeRects = mapSubs xmlDictToMRectangle xmlDict
      justsFilter (Just x) = True
      justsFilter Nothing = False
      filteredRects = filter justsFilter maybeRects 
      rects = map (\ (Just x) -> x) filteredRects 

      coord' = Rectangle (Coordinate 0 0) (Dimensions height' width') ()
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
