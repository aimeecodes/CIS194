{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("I":time:str)     -> LogMessage Info    (read time :: Int) (unwords str)
  ("W":time:str)     -> LogMessage Warning (read time :: Int) (unwords str)
  ("E":sev:time:str) -> LogMessage (Error (read sev:: Int)) (read time :: Int) (unwords str)
  _                  -> Unknown $ msg

parse :: String -> [LogMessage]
parse txt = map parseMessage $ lines txt

-- data MessageTree = Leaf
--                  | Node {
--                           leftchild  :: MessageTree
--                         , value      :: LogMessage
--                         , rightchild :: MessageTree
--                         }
--   deriving (Show, Eq)

-- data LogMessage = LogMessage {
--     messageType :: MessageType
--   , timeStamp   :: TimeStamp
--   , message     :: String
--   } | Unknown {message :: String}
--       deriving (Show, Eq)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree            -- unknown insertion case
insert logmessage Leaf  = Node Leaf logmessage Leaf
insert (LogMessage messagetype logtime str) (Node lc rootlogmessage rc)
  | logtime <= roottime = Node (insert (LogMessage messagetype logtime str) lc) rootlogmessage rc
  | logtime >  roottime = Node lc rootlogmessage (insert (LogMessage messagetype logtime str) rc)
    where
      roottime = timeStamp rootlogmessage

build :: [LogMessage] -> MessageTree
build []       = Leaf
build (m1:[])  = Node Leaf m1 Leaf
build (m1:ms)  = insert m1 (build ms)

-- foldlInsert :: [LogMessage] -> MessageTree
-- foldlInsert messages = foldl step Leaf messages
--   where
--     step acc m = insert m acc

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf rootlogmessage Leaf) = [rootlogmessage]
inOrder (Node lc rootlogmessage rc)     = (inOrder lc) ++ [rootlogmessage] ++ (inOrder rc)

wWW :: [LogMessage] -> [String]
wWW []      = []
wWW (m1:[]) = [parseErrors m1]
wWW ms = parseErrors n1 : wWW ns
  where
    (n1:ns) = inOrder $ build $ ms

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = results
  where
    nessages = wWW messages
    results  = [x | x <- nessages, not (null x)]

parseErrors :: LogMessage -> String
parseErrors (LogMessage (Error x) timestamp str)
  | x >= 50   = str
  | otherwise = []
parseErrors log = []


