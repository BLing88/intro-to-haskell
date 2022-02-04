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

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert msg@(LogMessage _ timeStamp _) (Node leftTree (LogMessage _ timeStampAtNode _) rightTree)
  | timeStamp <= timeStampAtNode = insert msg leftTree
  | otherwise = insert msg rightTree
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree msg rightTree) = inOrder leftTree ++ [msg] ++ inOrder rightTree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong msgs = [s | (LogMessage (Error level) _ s) <- msgs, level >= 50] 
