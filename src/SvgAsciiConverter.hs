module SvgAsciiConverter
( Coordinate(Coordinate)
, Dimensions(Dimensions)
, Rectangle(Rectangle)
, Painter(Painter)
, AsciiPicture(AsciiPicture)
, XmlParamKey
, XmlTag(XmlTag) 
, parseXml
, parseVBoxParameters
, mapToRectangle
, rectFloatToInt
, drawRectInt
, drawsegmentrow
) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

data Coordinate t = Coordinate t t deriving (Show)
data Dimensions t = Dimensions t t deriving (Show)
data Painter = Painter Char
data AsciiPicture = AsciiPicture [[Char]]
data Rectangle spaceT = Rectangle (Coordinate spaceT) (Dimensions spaceT) deriving (Show)

data XmlTag paramType = XmlTag String paramType [XmlTag paramType] deriving (Show)
type XmlParamKey = (String, String)

instance Functor XmlTag where
    fmap f (XmlTag name x []) = XmlTag name (f x) []
    fmap f (XmlTag name x subs) = XmlTag name (f x) (map (fmap f) subs) 

parseXml :: String -> Either ParseError [XmlTag [XmlParamKey]] 
parseXml x = parse xmlFile "xmlfile" x

xmlFile :: GenParser Char st [XmlTag [XmlParamKey]]
xmlFile = do
    spaces
    (try xmlHeader) <?> "xmlHeader"
    elements <- manyTill (between spaces spaces $ xmlElement "file") eof 
    return elements 

xmlElement :: String -> GenParser Char st (XmlTag [XmlParamKey])
xmlElement parent = do
    name <- xmlBeginTag parent
    spaces
    params <- manyTill (do {p <- xmlTagParam; spaces; return p} )  
                       (lookAhead $ oneOf "/>") 
    let uninterestingText = many (noneOf "<>") 
    slashOrBracket <- oneOf "/>"
                      <?> "xmltagbegin slashorbracket, / or > for " ++ name
    subtags <- case slashOrBracket of '/' -> do
                                             char '>'
                                             return []
                                      '>' -> do
                                             uninterestingText
                                             manyTill (between spaces spaces $ xmlElement name)
                                                      (xmlTagEnd name)
    return $ XmlTag name params subtags

xmlBeginTag :: String -> GenParser Char st String
xmlBeginTag parent = do
    try (char '<') 
        <?> "xmlbegintag < where parent is " ++ parent
    let tagname = many1 $ noneOf (controlChars ++ " \n")
    try tagname 
        <?> "xmlbegintag name, parent is " ++ parent 

xmlTagParam :: GenParser Char st XmlParamKey 
xmlTagParam = do
    let nameString = many1 $ noneOf (controlChars ++ " \n")
    name <- try nameString <?> "xmltagparam namestring" 
    spaces
    try (char '=') <?> "xmltagparam ="
    spaces
    try (char '\"') <?> "xmltagparam first \""
    let valueString = many $ noneOf "\""
    value <- try valueString <?> "xmltagparam valuestring" 
    try (char '\"') <?> "xmltag last \""
    return (name, value)

xmlTagEnd :: String -> GenParser Char st String
xmlTagEnd name = do
    let symbol = "</" ++ name ++ ">"
    try (string symbol)
        <?> "xmltagend " ++ symbol
       
controlChars = "<>=/" 
     
xmlHeader :: GenParser Char st String 
xmlHeader = do
    string "<?xml"
    many $ noneOf "<>?"
    string "?>"

-----------------------------------------------------------

vBoxParameters :: GenParser Char st (Rectangle Float) 
vBoxParameters = do
                 spaces 
                 let numberString  = do
                                     sign <- string "-"
                                             <|> return []
                                     largedigits <- many $ oneOf "0123456789"
                                     decimal <- string "."
                                                <|> return []
                                     smalldigits <- many $ oneOf "0123456789"
                                     return $ sign ++ largedigits ++ decimal ++ smalldigits
                     number = read `fmap` numberString
                 x1 <- number
                 spaces
                 y1 <- number
                 spaces
                 x2 <- number
                 spaces
                 y2 <- number
                 let coord = Coordinate x1 y1
                     dim = Dimensions (y2 - y1) (x2 - x1)
                 return $ Rectangle coord dim
                
parseVBoxParameters :: String -> Either ParseError (Rectangle Float)  
parseVBoxParameters input = parse vBoxParameters "(unknown)" input

-----------------------------------------------------------

mapToRectangle :: (Map.Map String String) -> Maybe (Rectangle Float) 
mapToRectangle map = Rectangle <$> maybeCoord <*> maybeDim 
    where readMap = (fmap read) . (\ x -> Map.lookup x map)
          maybeCoord = Coordinate <$> readMap "y" <*> readMap "x"
          maybeDim = Dimensions <$> readMap "height" <*> readMap "width"

rectFloatToInt :: (Rectangle Float) -> (Rectangle Int) -> (Rectangle Float) -> (Rectangle Int) 
rectFloatToInt coord coord' rectangle = Rectangle (Coordinate i' j') (Dimensions height' width') 
    where (Rectangle (Coordinate oi oj) (Dimensions dimi dimj) ) = coord
          (Rectangle (Coordinate oi' oj') (Dimensions dimi' dimj') ) = coord'
          (Rectangle (Coordinate i j) (Dimensions height width)) = rectangle
          iscale = (fromIntegral dimi') / dimi
          jscale = (fromIntegral dimj') / dimj
          height' = floor $ height * iscale 
          width' = floor $ width * jscale 
          i' = floor $ (fromIntegral oi') + (i - oi) * iscale
          j' = floor $ (fromIntegral oj') + (j - oj) * jscale 

drawRectInt :: (Rectangle Int) -> Painter -> AsciiPicture -> AsciiPicture
drawRectInt (Rectangle corner dim) p (AsciiPicture rows) = 
    AsciiPicture $ untouchedrows1 ++ paintedrows ++ untouchedrows2 
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
