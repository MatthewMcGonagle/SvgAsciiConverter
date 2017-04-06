module SvgAsciiConverter
( Coordinate(Coordinate)
, Dimensions(Dimensions)
, Rectangle(Rectangle)
, Painter(Painter)
, AsciiPicture(AsciiPicture)
, XmlParamKey
, XmlTag(XmlTag, name, params, subs) 
, parseXml
, parseVBoxParameters
, parseStyleKeys
, filterInterestingSubs
, mapParams
, mapSubs
, mapToMRectangle
, rectFloatToInt
, drawRectInt
, drawsegmentrow
) where

import Text.ParserCombinators.Parsec
import Control.Monad
import qualified Data.Map as Map

data Coordinate t = Coordinate 
    { x :: t
    , y :: t
    } deriving (Show)

data Dimensions t = Dimensions 
    { dimx :: t
    , dimy :: t
    } deriving (Show)

data Painter = Painter Char
data AsciiPicture = AsciiPicture [[Char]]
data Rectangle spaceT propertyT = Rectangle 
    { coord :: Coordinate spaceT 
    , dim   :: Dimensions spaceT 
    , prop  :: propertyT
    } deriving (Show)

data XmlTag paramType = XmlTag 
    { name   :: String 
    , params :: paramType 
    , subs   :: [XmlTag paramType] 
    } deriving (Show)

type XmlParamKey = (String, String)

instance Functor XmlTag where
    fmap f (XmlTag name x subs) = XmlTag name (f x) (map (fmap f) subs) 

instance Functor (Rectangle spaceT) where
    fmap f (Rectangle coord dim prop) = Rectangle coord dim (f prop)

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

vBoxParameters :: GenParser Char st (Rectangle Float ()) 
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
                 return $ Rectangle coord dim ()
                
parseVBoxParameters :: String -> Either ParseError (Rectangle Float ())  
parseVBoxParameters input = parse vBoxParameters "(unknown)" input

----------------------------------------------------------

styleParamKey :: GenParser Char st (String, String)
styleParamKey = do 
    name <- many1 $ noneOf ":"
    char ':'
    value <- many $ noneOf ";"
    return (name, value)

styleParamKeyList = styleParamKey `sepBy` (char ';')

parseStyleKeys :: String -> Either ParseError [(String, String)]
parseStyleKeys params = parse styleParamKeyList "(unknown)" params

-----------------------------------------------------------

filterInterestingSubs :: (paramType -> Bool) -> ((XmlTag [paramType]) -> Bool) -> XmlTag [paramType] -> XmlTag [paramType]
filterInterestingSubs paramIsInteresting subIsInteresting (XmlTag name params subs) = 
    (XmlTag name params) 
    . (map paramFilter)
    . (filter subIsInteresting)
    $ subs 
    where paramFilter (XmlTag n p s) = XmlTag n (filter paramIsInteresting p) s

mapParams :: (paramType -> paramType) -> XmlTag paramType -> XmlTag paramType
mapParams f (XmlTag name params subs) = XmlTag name (f params) subs

mapSubs :: (XmlTag paramType -> XmlTag paramType) -> XmlTag paramType -> XmlTag paramType 
mapSubs f (XmlTag name params subs) = XmlTag name params (map f subs)
 
mapToMRectangle :: (Map.Map String String) -> Maybe (Rectangle Float ())
mapToMRectangle map = Rectangle <$> maybeCoord <*> maybeDim <*> return () 
    where readMap = (fmap read) . (\ x -> Map.lookup x map)
          maybeCoord = Coordinate <$> readMap "y" <*> readMap "x"
          maybeDim = Dimensions <$> readMap "height" <*> readMap "width"


rectFloatToInt :: (Rectangle Float pT) -> (Rectangle Int pT') -> (Rectangle Float pT'') -> (Rectangle Int pT'') 
rectFloatToInt coord coord' rectangle = Rectangle (Coordinate i' j') (Dimensions height' width') props 
    where (Rectangle (Coordinate oi oj) (Dimensions dimi dimj) _ ) = coord
          (Rectangle (Coordinate oi' oj') (Dimensions dimi' dimj') _ ) = coord'
          (Rectangle (Coordinate i j) (Dimensions height width) props) = rectangle
          iscale = (fromIntegral dimi') / dimi
          jscale = (fromIntegral dimj') / dimj
          height' = floor $ height * iscale 
          width' = floor $ width * jscale 
          i' = floor $ (fromIntegral oi') + (i - oi) * iscale
          j' = floor $ (fromIntegral oj') + (j - oj) * jscale 

drawRectInt :: (Rectangle Int pT) -> Painter -> AsciiPicture -> AsciiPicture
drawRectInt (Rectangle corner dim _) p (AsciiPicture rows) = 
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
