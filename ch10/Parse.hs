module Parse where

import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Either

data ParseState = ParseState
  { string :: L.ByteString
  , offset :: Int64
  } deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

newtype Parser a = Parser { runParser :: ParseState -> Either String (a, ParseState) }

identity :: a -> Parser a
identity a = Parser (\s -> Right (a, s))

parse :: Parser a -> L.ByteString -> Either String a
parse parser initState =
  case runParser parser (ParseState initState 0) of
    Left err -> Left err
    Right (result, _) -> Right result
      
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
  initState { offset = newOffset }

parseByte :: Parser Word8
parseByte =
  getState ==> \initState ->
  case L.uncons (string initState) of
    Nothing -> bail "no more input"
    Just (byte, remainder) ->
      putState newState ==> \_ ->
      identity byte
      where newState = initState { string = remainder
                                 , offset = newOffset }
            newOffset = offset initState + 1

getState :: Parser ParseState
getState = Parser (\s -> Right (s, s))

putState :: ParseState -> Parser ()
putState = Parser (\_ -> Right ((), s))

(==>) :: Parser a -> (a -> Parser b) -> Parser b
firstParser ==> secondParser = Parser chainedParser
  where chainedParser initState =
          case runParser firstParser initState of
            Left err -> Left err
            Right (firstResult, newState) ->
              runParser (secondParser firstResult) newState
