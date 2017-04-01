module SvgAsciiConverter
( Coordinate(Coordinate)
, Dimensions(Dimensions)
, Rectangle(Rectangle)
, Painter(Painter)
, AsciiPicture(AsciiPicture)
, SvgElement(ViewBox, SvgRectangle)
, XmlParamKey
, XmlTag(XmlTag) 
, parseXml
, parseSvg
, mapToRectangle
, rectchange
, svgrecttorect
, viewboxtorect
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
    (try xmlheader) <?> "xmlheader"
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
    -- try (string "</")
    --     <?> "xmltagend </ for " ++ name
    -- try (string name)
    --     <?> "xmltagend identifier " ++ name
    -- try (string ">") 
    --     <?> "xmltagend > for " ++ name
        
controlChars = "<>=/" 
     
----------------------------------------------
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

-----------------------------------------------------------

mapToRectangle :: (Map.Map String String) -> Maybe (Rectangle Float) 
mapToRectangle map = Rectangle <$> maybeCoord <*> maybeDim 
    where readMap = (fmap read) . (\ x -> Map.lookup x map)
          maybeCoord = Coordinate <$> readMap "y" <*> readMap "x"
          maybeDim = Dimensions <$> readMap "height" <*> readMap "width"

rectchange :: (Rectangle Float) -> (Rectangle Int) -> (Rectangle Float) -> (Rectangle Int) 
rectchange coord coord' rectangle = Rectangle (Coordinate i' j') (Dimensions height' width') 
    where (Rectangle (Coordinate oi oj) (Dimensions dimi dimj) ) = coord
          (Rectangle (Coordinate oi' oj') (Dimensions dimi' dimj') ) = coord'
          (Rectangle (Coordinate i j) (Dimensions height width)) = rectangle
          iscale = (fromIntegral dimi') / dimi
          jscale = (fromIntegral dimj') / dimj
          height' = floor $ height * iscale 
          width' = floor $ width * jscale 
          i' = floor $ (fromIntegral oi') + (i - oi) * iscale
          j' = floor $ (fromIntegral oj') + (j - oj) * jscale 

viewboxtorect :: SvgElement -> Rectangle Float 
viewboxtorect (ViewBox (Coordinate j1 i1) (Coordinate j2 i2)) = 
    Rectangle (Coordinate i1 j1) (Dimensions (i2 - i1) (j2 - j1)) 

svgrecttorect:: SvgElement -> Rectangle Float
svgrecttorect (SvgRectangle r) = r

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
