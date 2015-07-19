module Array2d where

{--
  A simple module exposing an interface for 2d arrays implemented
  on top of the Array module.
-}

import Array exposing ( Array )
import List exposing (take, drop)

-- 2d Array Helpers

type alias Array2d a = (Int, Int, Array a)

repeat : Int -> Int -> a -> Array2d a
repeat nRows nCols val = (nRows, nCols, Array.repeat (nRows * nCols) val)

get : Int -> Int -> Array2d a -> Maybe a
get rowIdx colIdx (nRows, _, theArray) = Array.get (nRows * rowIdx + colIdx) theArray

set : Int -> Int -> a -> Array2d a -> Array2d a
set rowIdx colIdx val (nRows, nCols, theArray) = (nRows, nCols, Array.set (nRows * rowIdx + colIdx) val theArray)

{--
  Helper function to partition a list into a list of lists of length at most n.
--}

partition : Int -> List a -> List (List a)
partition n list =
  if n < 0 then [] else
    case list of
      [] -> []
      _ -> (take n list) :: (partition n (drop n list))

nRows : Array2d a -> Int
nRows (nRows, _, _) = nRows

nCols : Array2d a -> Int
nCols (_, nCols, _) = nCols

{--
  Return the array contents as an ordered list of lists, where each list represents
  a row of values.
--}
asRows : Array2d a -> List (List a)
asRows (_, nCols, theArray) = (partition nCols (Array.toList theArray))

asCols : Array2d a -> List (List a)
asCols (nRows, _, theArray) = (partition nRows (Array.toList theArray))

map : (a -> b) -> Array2d a -> Array2d b
map f (nRows, nCols, theArray) = (nRows, nCols, (Array.map f theArray))

indexedMap : (Int -> Int -> a -> b) -> Array2d a -> Array2d b
indexedMap f (nRows, nCols, theArray) =
  let
    g = (\ i v -> (f (i // nCols) (i % nCols) v))
  in
    (nRows, nCols, (Array.indexedMap g theArray))

foldl : (a -> b -> b) -> b -> Array2d a -> b
foldl f acc (_, _, theArray) = Array.foldl f acc theArray

foldr : (a -> b -> b) -> b -> Array2d a -> b
foldr f acc (_, _, theArray) = Array.foldr f acc theArray

{--
  Return the array contents as an ordered list of lists, where each list represents
  a column of values.
--}

testArray : Array2d Int
testArray = repeat 10 10 0
