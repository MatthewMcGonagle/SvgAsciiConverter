module SvgAsciiConverter
( Coordinate(Coordinate)
, Dimensions(Dimensions)
, Rectangle(Rectangle)
, Painter(Painter)
, AsciiPicture(AsciiPicture)
, SvgElement(ViewBox, SvgRectangle)
, XmlParam(XmlParam)
, XmlParamKey
, XmlTag(XmlTag) 
, parseXml
, parseSvg
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

data XmlDictValue = XmlDictParam String | XmlDictSub XmlDict
data XmlDict = XmlDict (Map.Map String (Map.Map String XmlDictValue)) 
data XmlTag t = XmlTag String t [XmlTag t] deriving (Show)
data XmlParam = XmlParam String String deriving (Show)
type XmlParamKey = (String, String)
data XmlTagString = XmlTagString String [(String, String)]

instance Functor XmlTag where
    fmap f (XmlTag name x []) = XmlTag name (f x) []
    fmap f (XmlTag name x subs) = XmlTag name (f x) (map (fmap f) subs) 

parseXml :: String -> Either ParseError [XmlTag [XmlParamKey]] 
parseXml x = parse xmlfile "xmlfile" x

xmlfile :: GenParser Char st [XmlTag [XmlParamKey]]
xmlfile = do
    spaces
    (try xmlheader) <?> "xmlheader"
    elements <- manyTill (between spaces spaces $ xmlelement "file") eof 
    return elements 

xmlelement :: String -> GenParser Char st (XmlTag [XmlParamKey])
xmlelement parent = do
    name <- xmlbegintag parent
    spaces
    params <- manyTill (do {p <- xmltagparam; spaces; return p} )  
                       (lookAhead $ oneOf "/>") 
    let uninterestingtext = many (noneOf "<>") 
    slashorbracket <- oneOf "/>"
                      <?> "xmltagbegin slashorbracket, / or > for " ++ name
    subtags <- case slashorbracket of '/' -> do
                                             char '>'
                                             return []
                                      '>' -> do
                                             uninterestingtext
                                             manyTill (between spaces spaces $ xmlelement name)
                                                      (xmltagend name)
    return $ XmlTag name params subtags

xmlbegintag :: String -> GenParser Char st String
xmlbegintag parent = do
    try (char '<') 
        <?> "xmlbegintag < where parent is " ++ parent
    let tagname = many1 $ noneOf (controlchars ++ " \n")
    try tagname 
        <?> "xmlbegintag name, parent is " ++ parent 

xmltagparam :: GenParser Char st XmlParamKey 
xmltagparam = do
    let namestring = many1 $ noneOf (controlchars ++ " \n")
    name <- try namestring <?> "xmltagparam namestring" 
    spaces
    try (char '=') <?> "xmltagparam ="
    spaces
    try (char '\"') <?> "xmltagparam first \""
    let valuestring = many $ noneOf "\""
    value <- try valuestring <?> "xmltagparam valuestring" 
    try (char '\"') <?> "xmltag last \""
    return (name, value)

xmltagend :: String -> GenParser Char st String
xmltagend name = do
    let symbol = "</" ++ name ++ ">"
    try (string symbol)
        <?> "xmltagend " ++ symbol
    -- try (string "</")
    --     <?> "xmltagend </ for " ++ name
    -- try (string name)
    --     <?> "xmltagend identifier " ++ name
    -- try (string ">") 
    --     <?> "xmltagend > for " ++ name
        
controlchars = "<>=/" 
-- xmlregulartext = many $ noneOf "<"

-- xmlformatting = many $ oneOf " \n" 
     
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
viewboxtorect (ViewBox (Coordinate i1 j1) (Coordinate i2 j2)) = 
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
