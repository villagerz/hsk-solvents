{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import BasicPrelude (readMay)
import Data.Text (pack)

parseMsg :: String -> LogMessage
parseMsg full@('E' : s) =
  case createPossibleError c ss of
    Just l -> l
    Nothing -> Unknown full
  where (c : ss) = words s
  
parseMsg full@('I' : s) = createInfoOrWarning (LogMessage Info) (withTimestamp (words s)) full
parseMsg full@('W' : s) = createInfoOrWarning (LogMessage Warning) (withTimestamp (words s)) full
parseMsg s = Unknown s

createInfoOrWarning :: (TimeStamp -> String -> LogMessage) -> Maybe (Int, String) -> String -> LogMessage
createInfoOrWarning l (Just (t,s)) _ = l t s
createInfoOrWarning _ Nothing s = Unknown s

createPossibleError :: String -> [String] -> Maybe LogMessage
createPossibleError c line =
  case readMay (pack c) of
    Just code -> case withTimestamp line of
                   Just (time, log') -> Just (LogMessage (Error code) time log')
                   Nothing -> Nothing
    Nothing -> Nothing

withTimestamp :: [String] -> Maybe (Int, String)
withTimestamp (t : s) =
  case readMay (pack t) of
    Just time -> Just (time, unwords s)
    Nothing -> Nothing
withTimestamp [] = Nothing

parse :: String -> [LogMessage]
parse l = map parseMsg (lines l)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert l (Node left top right)
  | tstmp l <= tstmp top = Node (insert l left) top right
  | otherwise = Node left top (insert l right)
  where tstmp (LogMessage _ t _) = t
        tstmp (Unknown _) = error "oopsy"

insert l Leaf = Node Leaf l Leaf

build :: [LogMessage] -> MessageTree
build ls = build' ls Leaf
           where build' (l : lls) t = build' lls (insert l t)
                 build' [] t = t

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node l top r) = (inOrder l) ++ [top] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = messages (inOrder (build (errorsAbove 40 l)))
  where errorsAbove n ls = [msg | msg <- ls, errorAbove n msg]
          where errorAbove nn (LogMessage (Error m) _ _ ) = m >= nn
                errorAbove _ _ = False
        messages msgs = [getMsg x | x <- msgs]
          where getMsg (LogMessage _ _ s) = s
                getMsg _ = ""
        
