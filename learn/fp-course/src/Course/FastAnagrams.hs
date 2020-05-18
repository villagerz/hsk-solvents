{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams s f = (map ncString) <$>
  (filter (isAnagramIn ps ) ) <$> (ncstrings . lines <$> readFile f)
  where ps = S.fromList . hlist . permutations' $ s

-- mapFunc <$> readFile path
--     where mapFunc content = 
--            filter (`S.member` (S.fromList . hlist $ lines content)) (permutations name)

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

instance Ord NoCaseString where
  (<=) = (<=) `on`  map toLower . ncString 

isAnagramIn :: S.Set NoCaseString -> NoCaseString -> Bool
isAnagramIn l s = S.member s l
  
ncstrings :: List Chars -> List NoCaseString
ncstrings l = map NoCaseString l

permutations' :: Chars -> List NoCaseString
permutations'  = ncstrings .  permutations 
