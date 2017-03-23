module SvgAsciiConverter
( Coordinate(Coordinate)
, Painter(Painter)
, AsciiPicture(AsciiPicture)
, SvgElement(Rectangle)
, parseSvg
, drawrectangle 
, drawsegmentrow
) where

import Text.ParserCombinators.Parsec

data Coordinate = Coordinate Int Int
data Painter = Painter Char
data AsciiPicture = AsciiPicture [[Char]]
data SvgElement = Rectangle deriving (Show)

text :: GenParser Char st String
text = many $ noneOf "<>"

xmlheader :: GenParser Char st String
xmlheader = do
    string "<?"
    many $ noneOf "<>?"
    string "?>"

svgfile :: GenParser Char st [SvgElement]
svgfile = do
    xmlheader
    return []

parseSvg :: String -> Either ParseError [SvgElement]
parseSvg file = parse svgfile "(unkown)" file

drawrectangle :: Coordinate -> Coordinate -> Painter -> AsciiPicture -> AsciiPicture
drawrectangle a b p (AsciiPicture rows) = AsciiPicture $ untouchedrows1 ++ paintedrows ++ untouchedrows2 
    where (Coordinate a1 a2) = a
          (Coordinate b1 b2) = b
          change2 = (b2 - a2) `div` (b1 - a1)
          remainder2 = (b2 - a2) `mod` (b1 - a1) 
          untouchedrows1 = take a1 rows 
          restrows = drop a1 rows 
          rowstochange = take (b1 - a1) restrows 
          untouchedrows2 = drop (b1 - a1) restrows 
          paintedrows = map (drawsegmentrow a2 b2 p ) rowstochange

-- Draws from position a to position b, not including b
drawsegmentrow :: Int -> Int -> Painter -> [Char] -> [Char]
drawsegmentrow a b (Painter p) row = unchanged1 ++ painted ++ unchanged2
    where unchanged1 = take a row
          restofrow = drop a row
          painted = take (b - a) $ repeat p
          unchanged2 = drop (b - a) restofrow
