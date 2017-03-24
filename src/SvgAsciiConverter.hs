module SvgAsciiConverter
( Coordinate(Coordinate)
, Dimensions(Dimensions)
, Rectangle(Rectangle)
, Painter(Painter)
, AsciiPicture(AsciiPicture)
, SvgElement(ViewBox, SvgRectangle)
, parseSvg
, drawrectangle 
, drawsegmentrow
) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

data Coordinate t = Coordinate t t deriving (Show)
data Dimensions t = Dimensions t t deriving (Show)
data Painter = Painter Char
data AsciiPicture = AsciiPicture [[Char]]
data Rectangle t = Rectangle (Coordinate t) (Dimensions t) deriving (Show)
data SvgElement = ViewBox (Coordinate Float) (Coordinate Float)
                | SvgRectangle (Rectangle Float) 
                deriving (Show)

text :: GenParser Char st String
text = many $ noneOf "<>"

formatting = many $ oneOf " \n"
endline = char '\n'

xmlheader :: GenParser Char st String 
xmlheader = do
    string "<?xml"
    many $ noneOf "<>?"
    string "?>"

viewboxparam :: GenParser Char st [Float]
viewboxparam = do
    numstrs <- (floatingnumber) `sepBy` (char ' ')
    return $ map read numstrs
    where floatingnumber = many1 $ noneOf " "

svgstart :: GenParser Char st SvgElement 
svgstart = do
    string "<svg"
    endline
    ps <- (spaces >> parameter) `sepBy` endline 
    string ">"
    let pdict = Map.fromList ps
        Just vboxstring = Map.lookup "viewBox" pdict
        Right [vbox1, vbox2, vbox3, vbox4] = parse viewboxparam "(unknown)" vboxstring
        start = Coordinate vbox1 vbox2
        end = Coordinate vbox3 vbox4  
    return $ ViewBox start end 

defs :: GenParser Char st String
defs = do
    string "<defs"
    many $ noneOf "/>"
    string "/>"

metadatastart :: GenParser Char st String
metadatastart = do
    string "<metadata"
    endline
    text
    string ">"

metadata :: GenParser Char st String
metadata = do
    metadatastart
    manyTill anyChar $ (try $ string "</metadata>")
    return []
    
parameter :: GenParser Char st (String, String) 
parameter = do
    name <- many $ noneOf " =<>/"
    spaces
    char '='
    spaces
    char '\"'
    value <- many $ noneOf "\""
    char '\"'
    return $ (name, value) 
    
    
rect :: GenParser Char st SvgElement
rect = do
    string "<rect"
    endline
    ps <- (spaces >> parameter) `sepBy` endline 
    spaces
    string "/>"
    let pdict = Map.fromList ps
        Just x = Map.lookup "x" pdict
        Just y = Map.lookup "y" pdict
        Just height = Map.lookup "height" pdict
        Just width = Map.lookup "width" pdict
        corner = Coordinate (read y) (read x)
        dim = Dimensions (read height) (read width)
    return $ SvgRectangle (Rectangle corner dim)
    
    
svgfile :: GenParser Char st [SvgElement]
svgfile = do
    xmlheader
    formatting
    viewbox <- svgstart
    formatting 
    defs
    formatting
    metadata >> endline
    rectlist <- (spaces >> (try rect)) `endBy` endline
    string "</svg>" >> endline
    eof
    return $ viewbox : rectlist 

parseSvg :: String -> Either ParseError [SvgElement]
parseSvg file = parse svgfile "(unkown)" file

drawrectangle :: (Rectangle Int) -> Painter -> AsciiPicture -> AsciiPicture
drawrectangle (Rectangle corner dim) p (AsciiPicture rows) = AsciiPicture $ untouchedrows1 ++ paintedrows ++ untouchedrows2 
    where (Coordinate i j) = corner 
          (Dimensions dimi dimj) = dim 
          untouchedrows1 = take i rows 
          restrows = drop i rows 
          rowstochange = take dimi restrows 
          untouchedrows2 = drop dimi restrows 
          j' = j + dimj
          paintedrows = map (drawsegmentrow j j' p) rowstochange

-- Draws from position a to position b, not including b
drawsegmentrow :: Int -> Int -> Painter -> [Char] -> [Char]
drawsegmentrow a b (Painter p) row = unchanged1 ++ painted ++ unchanged2
    where unchanged1 = take a row
          restofrow = drop a row
          painted = take (b - a) $ repeat p
          unchanged2 = drop (b - a) restofrow