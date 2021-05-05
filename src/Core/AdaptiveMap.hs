
module Core.AdaptiveMap
  (AdaptiveMap,
   empty, insert, lookup,
   toList, fromList
  ) where

import Prelude hiding (lookup)
import qualified Prelude
import qualified Data.HashMap.Strict as H
import Data.Hashable

data AdaptiveMap k v =
      Single Int !k !v
    | List Int ![(k,v)]
    | Hash !(H.HashMap k v)

empty :: Int -> AdaptiveMap k v
empty listCapacity = List listCapacity []

insertList :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
insertList k v [] = [(k,v)]
insertList k v ((k1,v1) : rest)
  | k == k1 = (k,v) : rest
  | otherwise = (k1,v1) : insertList k v rest

insert :: (Eq k, Hashable k) => k -> v -> AdaptiveMap k v -> AdaptiveMap k v
insert k v (Single sz k1 v1)
  | (k == k1) || sz == 1 = Single sz k v
  | otherwise = List sz [(k,v), (k1,v1)]
insert k v (List sz []) = Single sz k v
insert k v (List sz lst)
  | length lst +1 > sz = Hash $ H.fromList $ insertList k v lst
  | otherwise = List sz $ insertList k v lst
insert k v (Hash hsh) = Hash $ H.insert k v hsh

lookup :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> Maybe v
lookup k (Single _ k1 v1)
  | k == k1 = Just v1
  | otherwise = Nothing
lookup k (List _ lst) = Prelude.lookup k lst
lookup k (Hash hsh) = H.lookup k hsh

fromList :: (Eq k, Hashable k) => Int -> [(k,v)] -> AdaptiveMap k v
fromList listCapacity [(k,v)] = Single listCapacity k v
fromList listCapacity lst
  | length lst > listCapacity = Hash $ H.fromList lst
  | otherwise = List listCapacity lst

toList :: AdaptiveMap k v -> [(k,v)]
toList (Single _ k v) = [(k,v)]
toList (List _ lst) = lst
toList (Hash hsh) = H.toList hsh

