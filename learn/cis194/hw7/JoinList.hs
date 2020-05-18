{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinList where
import Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l +++ r = Append (tag l <> tag r) l r 

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a ) = Just a
indexJ i (Single _ _ ) = Nothing
indexJ i t@(Append m l r)
  | i < getSize(size (tag t)) - 1 = indexJ i l
  | otherwise = indexJ ((i+1) - getSize(size (tag t))) r


joinlist :: String -> JoinList Size Char
joinlist = foldl (\t c -> t +++ (Single (1::Size) c)) Empty
