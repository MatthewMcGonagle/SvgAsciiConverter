module Main where

import SvgAsciiConverter
import System.IO
import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  handle <- openFile "example.svg" ReadMode
  contents <- hGetContents handle

  xml <- case parseXml contents 
        of Left parseError -> error (show parseError) 
           Right x -> return x 

  xmlSvg <- case List.find (\ x -> name x `elem` ["svg"]) xml
        of Nothing -> error "Couldn't find svg tag in list of tags"
           Just x -> return x

  vBoxString <- case Map.lookup "viewBox" (Map.fromList $ params xmlSvg) 
        of Nothing -> error "viewBox parameter not found"
           Just x -> return x

  vBox <- case parseVBoxParameters vBoxString 
        of Left parseError -> error (show parseError)
           Right x -> return x 

  let xmlDict = xmlDict' 
        { subs = map removeUninterestingP subs'}  
        where xmlDict' = Map.fromList `fmap` xmlSvg
              removeUninterestingP x = x 
                                       { params = Map.filterWithKey pIsInteresting (params x) 
                                       } 
              pIsInteresting k v = k `elem` ["x", "y", "height","width", "style"] 
              subs' = filter (\ x -> name x `elem` ["rect"]) $ subs xmlDict'
              tagIsInteresting x = name x `elem` ["rect"]

  let getMRect tag = do
            let pMap = params tag
            plessRect <- mapToMRectangle pMap
            styles <- Map.lookup "style" pMap
            styleKeys <- case parseStyleKeys styles
                    of Left error -> Nothing
                       Right x -> Just x
            let styleMap = Map.fromList styleKeys
            fillstyle <- Map.lookup "fill" styleMap 
            return $ (\_ -> fillstyle) `fmap` plessRect
     
  rectList <- case mapM getMRect (subs xmlDict) of
    Nothing -> error "Problem turning parameter maps into rectangles" 
    Just x -> return x 
               
  let coord' = Rectangle (Coordinate 0 0) (Dimensions height' width') ()
      height' = 60
      width' = 100
      rectMap x = rectFloatToInt vBox coord' x
      rectInts = map rectMap rectList 

  putStrLn "After coordinate conversion, the rectange list = "
  print rectInts 

  let canvas' = AsciiPicture asciiarray
        where asciiarray = take height' $ repeat asciirow
              asciirow = take width' $ repeat ' '
      pictureMap = foldl (\acc x -> drawRectInt x (Painter '*') acc) canvas'
      (AsciiPicture painting') = pictureMap rectInts 
  mapM_ putStrLn painting'

  hClose handle
