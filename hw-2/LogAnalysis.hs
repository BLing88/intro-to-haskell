module LogAnalysis where

import Log

parseMessageType :: String -> Maybe (MessageType, String)
parseMessageType s = 
  case words s of
    ("I" : cs) -> Just (Info, unwords cs)
    ("W" : cs) -> Just (Warning, unwords cs) 
    ("E" : level : cs) -> Just (Error (read level :: Int), unwords cs)
    _ -> Nothing

parseTimeStamp :: String -> Maybe (TimeStamp, String)
parseTimeStamp s = 
  case words s of
    (time : cs) -> Just (read time :: Int, unwords cs)
    _ -> Nothing

parseMaybeMessage :: String -> Maybe LogMessage
parseMaybeMessage s = do
  (messageType, restWithTimeStamp) <- parseMessageType s
  (timestamp, rest) <- parseTimeStamp restWithTimeStamp
  pure $ LogMessage messageType timestamp rest

parseMessage :: String -> LogMessage
parseMessage s =
  case parseMaybeMessage s of
    (Just logMessage) -> logMessage
    Nothing -> Unknown s
-- parseMessage s =  
--   case parseMessageType s of
--     Just (messageType, restWithTimestamp) -> 
--       case parseTimeStamp restWithTimestamp of
--         Just (timestamp, rest) -> LogMessage messageType timestamp rest
--         Nothing -> Unknown s
--     Nothing -> Unknown s

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

