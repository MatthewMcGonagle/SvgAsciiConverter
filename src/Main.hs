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
      eitherXmlSvg = head `fmap` eitherXml 
      eitherVBox = do
                   xmlSvg <- eitherXmlSvg
                   let (XmlTag _ ps _) = xmlSvg
                       pMap = Map.fromList ps
                       maybeVBoxPString = Map.lookup "viewBox" pMap
                   case maybeVBoxPString of Nothing -> parse (unexpected "viewBox Parameter Missing") "" ""
                                            Just x -> parseVBoxParameters x 
  putStrLn "eitherVBox = "
  print eitherVBox
  let eitherXmlFiltered = do
                          xmlSvg <- eitherXmlSvg
                          let (XmlTag name ps subs) = xmlSvg
                              subs' = filter (\ (XmlTag n _ _) -> n `elem` ["rect"]) subs
                              xmlSvg' = XmlTag name ps subs'
                          let xmlSvg'' = (filter interestingP) `fmap` xmlSvg'
                              interestingP (key, value) = key `elem` ["x", "y", "height", "width", "style"] 
                          return xmlSvg''

  let eitherXmlDict = do
                xmlFiltered <- eitherXmlFiltered
                return $ Map.fromList `fmap` xmlFiltered 

  let eitherRects = do
                    xmlDict <- eitherXmlDict
                    let (XmlTag _ _ subs) = xmlDict
                        rects = map (\ (XmlTag _ ps _) -> mapToRectangle ps) subs 
                        justsFilter (Just x) = True
                        justsFilter Nothing = False
                        rects' = filter justsFilter rects 
                    return $ map (\ (Just x) -> x) rects'
  putStrLn "eitherRects = "
  print eitherRects 
                        
  let coord' = Rectangle (Coordinate 0 0) (Dimensions height' width')
      height' = 60
      width' = 100
      eitherRectMap x = rectFloatToInt <$> eitherVBox <*> Right coord' <*> x
      eitherRectInts = eitherRects >>= mapM (eitherRectMap . Right) 
  putStrLn "After coordinate conversion, the rectange list = "
  print eitherRectInts 

  let canvas' = AsciiPicture asciiarray
      asciiarray = take height' $ repeat asciirow
      asciirow = take width' $ repeat ' '
      pictureMap = foldl (\acc x -> drawRectInt x (Painter '*') acc) canvas'
      (Right (AsciiPicture painting')) = pictureMap `fmap` eitherRectInts 
  mapM_ putStrLn painting'
  
  hClose handle
