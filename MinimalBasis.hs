module MinimalBasis where

import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Trie as D
import qualified Data.ByteString.Char8 as C

type Attr = C.ByteString {- attribute -}
type FD = ([Attr], [Attr])
data FDs = FDs [Attr] [FD] {- [Attr] - relation, [FD] - all FDs in the relation -} 
  deriving (Show, Eq)

{- split RHS of a FD into singletons -}
makeSingletonRHSHelper :: FD -> [FD]
makeSingletonRHSHelper (_,  []) = []
makeSingletonRHSHelper (strList, (x : xs)) = (strList, [x]) : makeSingletonRHSHelper (strList, xs)   

{- split RHS of [FD] into singletons -}
makeSingletonRHS :: [FD] -> [FD]
makeSingletonRHS [] = []
makeSingletonRHS (x : xs) = makeSingletonRHSHelper x ++ makeSingletonRHS xs 

{- determine if a list is a sub-list of another -}
isSublist :: Eq a => [a] {- supposed sublist -} ->
                                 [a] {- larger list -} ->
                                 Bool {- true if it is sub=list, false otherwise -}
isSublist (x : xs) largerList =
  if elem x largerList then
    isSublist xs largerList
  else
    False
isSublist [] largerList = True

{- insert an element into a list, does nothing if there is a duplicate -}
setInsert :: Eq a => a -> [a] -> [a]
setInsert a l = if elem a l then l else a : l

{- combine two lists into one, omitting duplicates -}
setCombine :: Eq a => [a] -> [a] -> [a]
setCombine [] l = l
setCombine (x : xs) l = setCombine xs newSet
  where newSet = setInsert x l
 
doClosure :: [FD] {- all FDs -} ->
                     [FD] {- available FDs -} ->
                     [Attr] {- Attrs started with -} ->
                     [Attr] {- Attrs already covered -} ->
                     [Attr] {- Attrs covered in closure -}
doClosure f [] a0 a =
  {- do another round of closure if more Attrs are enclosed -}
  if length a > length a0 then
    doClosure f f a a
  else
    a
doClosure f ((lhs, rhs) : xs) a0 a =
  if isSublist lhs a then
    (doClosure f xs a0 newEnclosed)
  else doClosure f xs a0 a
    where newEnclosed = setCombine rhs a

{- cited from https://stackoverflow.com/questions/127704/algorithm-to-return-all-combinations-of-k-elements-from-n/8626006#8626006 -}
nCr :: (Num t1, Eq t1) => t1 -> [t] -> [[t]]
nCr 0 lst = [[]]
nCr n lst = do {- list monad -}
    (x:xs) <- tails lst {- loop head -}
    rest   <- nCr (n-1) xs {- loop body -}
    return $ x : rest

{- find all combinations with no more than certain amount of elements -}
combinations :: Int {- #elements to select -} ->
                          [a] {- all elements -} ->
                          [[a]] {- all combinations -}
combinations 0 _ = [[]]
combinations n a = nCr n a ++ combinations (n - 1) a

{- return Just Attr if there is an extra Attr on LHS, Nothing otherwise -}
findLHSExtraAttr :: [Attr] {- Attr on LHS of an FD -} ->
                                [Attr] {- Attr found by closure -} ->
                                [Attr] {- Attr in the combination (started with) -} ->
                                Maybe Attr
findLHSExtraAttr [] closure combo = Nothing
findLHSExtraAttr (x : xs) closure combo =
  {- duplicate if attribute in closure and is not in set of starting Attributes -}
  if elem x closure && (not (elem x combo)) then
    Just x
  else
    findLHSExtraAttr xs closure combo

{- remove extra attributes from LHS of one FD -}
rmLHSExtraAttrHelper ::  [FD] {- All available FDs -} ->
                                          FD {- FD whose LHS is to be simplified -} ->
                                          [[Attr]] {- combinations yet to be tested -} ->
                                          (FD, [FD]) {- simplified FD and the updated FD -}
rmLHSExtraAttrHelper fs f [] = (f, fs)
rmLHSExtraAttrHelper fs (lhs, rhs) (x : xs) =
  if isJust e then
    let
      extraAttr = fromJust e
      newFD = ((delete extraAttr lhs), rhs)
      newAllFD = newFD : delete (lhs, rhs) fs
    in
      {- check if there are more redundant attributes -}
      rmLHSExtraAttr newAllFD newFD
  else
    rmLHSExtraAttrHelper fs (lhs, rhs) xs
  where
    closure = doClosure fs fs x x
    e = findLHSExtraAttr lhs closure x

{- remove extra attributes on LHS for one FD, generate combination for helper -}
rmLHSExtraAttr :: [FD] {- all FDs -} ->
                              FD {- FD whose LHS may have redundant Attr -} ->
                              (FD, [FD]) {- resulting FD and all FDs -}
rmLHSExtraAttr fs (lhs, rhs) = rmLHSExtraAttrHelper fs (lhs, rhs) c
  where
     c = combinations (length lhs) lhs

{- remove all extra Attrs on LHS of all FDs -}
rmAllLHSExtraAttr :: [FD] {- all FDs -} ->
                                   [FD] {- FDs whose LHS has not been reduced -} ->
                                   [FD] {- resulting FDs -}
rmAllLHSExtraAttr fs [] = []
rmAllLHSExtraAttr fs (x : xs) =
  nf : rmAllLHSExtraAttr nfs xs
  where
    t = rmLHSExtraAttr fs x
    nf = fst t
    nfs = snd t

{- removed redundant FDs from a set of FDs -}
rmRedundantFDsHelper :: [FD] {- all FDs -} ->
                                            [FD] {- FDs that may be redundant -} ->
                                            [FD] {- all FDs with no redundants -}
rmRedundantFDsHelper f [] = []
rmRedundantFDsHelper f ((lhs, rhs) : xs) =
  if isSublist rhs closure then
    r1
  else
    (lhs, rhs) : r 
  where
    f1 = delete (lhs, rhs) f
    closure = doClosure f1 f1 lhs lhs
    r = rmRedundantFDsHelper f xs
    r1 = rmRedundantFDsHelper f1 xs

{- remove redundant FDs from a set of FDs -}
rmRedundantFDs :: [FD] -> [FD]
rmRedundantFDs fs = rmRedundantFDsHelper fs fs

{- combine FDs that share LHS -}
uniteMinBasis :: [FD] -> [FD]
uniteMinBasis [] = []
uniteMinBasis [x] = [x]
uniteMinBasis ((lhs1, rhs1) : (lhs2, rhs2) : xs) =
  if lhs1 == lhs2 then uniteMinBasis ((lhs1, rhs1 ++ rhs2) : xs)
  else (lhs1, rhs1) : uniteMinBasis ((lhs2, rhs2) : xs)

{- put everything together and find the minmal basis of a set of FDs -}
findMinimalBasis :: [FD] -> [FD]
findMinimalBasis x = minimalCombinedBasis
  where
    singleton = makeSingletonRHS x {- step 1 -}
    reduced = rmAllLHSExtraAttr singleton singleton {- step 2 -}
    minimalSeparatedBasis = rmRedundantFDs reduced {- step 3 -}
    minimalCombinedBasis = uniteMinBasis minimalSeparatedBasis {- step 4 -}

{- indev code for cmobination, not working -}
comboCombineHelper :: a -> [[a]] -> [[a]]
comboCombineHelper e [] = [[]]
comboCombineHelper e (x : xs) = (e : x) : comboCombineHelper e xs 

comboCombine :: [[a]] -> [[a]]
comboCombine [] = []
comboCombine ([] : xss) = comboCombine xss 
comboCombine ((x :xs) : xss)= comboCombineHelper x (comboCombine (xs : xss))

combo :: Int -> [a] -> [[a]]
combo 0 _ = [[]]
combo n lst = comboCombine sets 
  where
    sets = replicate n lst
