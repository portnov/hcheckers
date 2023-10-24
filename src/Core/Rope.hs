
module Core.Rope where

import Prelude hiding (filter, any, sum, length, elem, notElem, null, mapM, zip)
import qualified Prelude
import qualified Data.Maybe
import Data.List (sortOn)

newtype Rope a = Rope [[a]]
  deriving (Eq, Show, Read)

rope :: [a] -> Rope a
rope [] = Rope []
rope lst = Rope [lst]

toList :: Rope a -> [a]
toList (Rope ys) = concat ys

empty :: Rope a
empty = Rope []

null :: Rope a -> Bool
null (Rope []) = True
null _ = False

cons :: a -> Rope a -> Rope a
cons x (Rope []) = Rope [[x]]
cons x (Rope (y:ys)) = Rope ((x:y):ys)

uncons :: Rope a -> Maybe (a, Rope a)
uncons (Rope []) = Nothing
uncons (Rope ([]:(x:xs):ys)) = Just (x, Rope ys)
uncons (Rope ([x]:ys)) = Just (x, Rope ys)
uncons (Rope ((x:xs):ys)) = Just (x, Rope (xs:ys))

instance Semigroup (Rope a) where
  (Rope []) <> (Rope ys2) = Rope ys2
  (Rope [[]]) <> (Rope ys2) = Rope ys2
  (Rope ys1) <> (Rope []) = Rope ys1
  (Rope ys1) <> (Rope [[]]) = Rope ys1
  (Rope ys1) <> (Rope ys2) = Rope (ys1 ++ ys2)

instance Monoid (Rope a) where
  mempty = empty
  mappend = (<>)

instance Functor Rope where
  fmap f (Rope []) = Rope []
  fmap f (Rope (y:ys)) = Rope (fmap f y : map (map f) ys)

instance Foldable Rope where
  foldMap _ (Rope []) = mempty
  foldMap fn (Rope (y:ys)) = foldMap fn y `mappend` foldMap fn (Rope ys)

mapM :: Monad m => (a -> m b) -> Rope a -> m (Rope b)
mapM _ (Rope []) = return $ Rope []
mapM fn (Rope (y:ys)) = do
  z <- Prelude.mapM fn y
  zs <- Prelude.mapM (Prelude.mapM fn) ys
  return $ Rope (z:zs)

filter :: (a -> Bool) -> Rope a -> Rope a
filter p (Rope ys) = Rope $ map (Prelude.filter p) ys

mapMaybe :: (a -> Maybe b) -> Rope a -> Rope b
mapMaybe fn (Rope ys) = Rope $ map (Data.Maybe.mapMaybe fn) ys

any :: (a -> Bool) -> Rope a -> Bool
any _ (Rope []) = False
any _ (Rope [[]]) = False
any fn (Rope (y:ys)) = Prelude.any fn y || or (map (Prelude.any fn) ys)

sum :: Num a => Rope a -> a
sum (Rope []) = 0
sum (Rope [[]]) = 0
sum (Rope ys) = Prelude.sum $ map Prelude.sum ys

length :: Rope a -> Int
length (Rope []) = 0
length (Rope ys) = Prelude.sum $ map Prelude.length ys

lengthAtLeast :: Rope a -> Int -> Bool
lengthAtLeast (Rope []) 0 = True
lengthAtLeast (Rope []) _ = False
lengthAtLeast (Rope ys) n =
  let notEmpty = Prelude.filter (not . Prelude.null) ys
  in  (Prelude.length notEmpty >= n) || listLengthAtLeast (concat ys) n

listLengthAtLeast :: [a] -> Int -> Bool
listLengthAtLeast [] 0 = True
listLengthAtLeast _ 0 = False
listLengthAtLeast [] _ = False
listLengthAtLeast (x:xs) n = listLengthAtLeast xs (n-1)

lengthLessThan :: Rope a -> Int -> Bool
lengthLessThan (Rope []) 0 = False
lengthLessThan (Rope []) _ = True
lengthLessThan (Rope ys) n =
  let notEmpty = Prelude.filter (not . Prelude.null) ys
  in  Prelude.length notEmpty < n

elem :: Eq a => a -> Rope a -> Bool
elem _ (Rope []) = False
elem _ (Rope [[]]) = False
elem x (Rope (y:ys)) = x `Prelude.elem` y || or (map (Prelude.elem x) ys)

notElem :: Eq a => a -> Rope a -> Bool
notElem _ (Rope []) = True
notElem _ (Rope [[]]) = True
notElem x (Rope (y:ys)) = x `Prelude.notElem` y && and (map (Prelude.notElem x) ys)

sortOnApprox :: Ord i => (a -> i) -> Rope a -> Rope a
sortOnApprox _ (Rope []) = Rope []
sortOnApprox fn (Rope ys) =
  let notEmpty = Prelude.filter (not . Prelude.null) ys
      fn' (x:_) = fn x
  in  Rope $ sortOn fn' ys

