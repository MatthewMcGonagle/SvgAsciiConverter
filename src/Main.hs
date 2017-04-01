module Main where

import SvgAsciiConverter
import System.IO
import Control.Monad
import qualified Data.Map as Map

main :: IO ()
main = do
  handle <- openFile "example.svg" ReadMode
  contents <- hGetContents handle
  let xmltags = parseXml contents
      eitherXmlSvg = head `fmap` xmltags
      eitherXmlFiltered = do
                              xmlSvg <- eitherXmlSvg
                              let (XmlTag name ps subs) = xmlSvg
                                  subs' = filter (\ (XmlTag n _ _) -> n `elem` ["rect"]) subs
                                  xmlSvg' = XmlTag name ps subs'
                              let xmlSvg'' = (filter interestingP) `fmap` xmlSvg'
                                  interestingP (key, value) = key `elem` ["x","y","height","width"] 
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
                    return $ map (\ (Just x) -> SvgRectangle x) rects'
  putStrLn "eitherRects = "
  print eitherRects 
                        

  let svgelems = parseSvg contents
  putStrLn "svgelems = "
  print svgelems
  --rectlist = drop 1 `fmap` svgelems
  let vbox = head `fmap` svgelems
  -- putStrLn "After dropping first element, rectlist= "
  -- print rectlist 
  putStrLn "The viewbox = "
  print vbox 
  let coord = viewboxtorect `fmap` vbox
      coord' = Rectangle (Coordinate 0 0) (Dimensions height' width')
      height' = 40
      width' = 80
  putStrLn "After converting from viewbox to rectangle, we have"
  print coord 
  let newrectlist = eitherRects >>= mapM (\x -> rectchange <$> coord <*> Right coord' <*> conversionmap x) 
      conversionmap = Right . svgrecttorect 
  putStrLn "After coordinate conversion, the rectange list = "
  print newrectlist
  let canvas' = AsciiPicture asciiarray
      asciiarray = take height' $ repeat asciirow
      asciirow = take width' $ repeat ' '
      (Right (AsciiPicture painting')) = foldl (\acc x -> drawrectangle x (Painter '*') acc) canvas' `fmap` newrectlist 
  mapM_ putStrLn painting'
  
  hClose handle
