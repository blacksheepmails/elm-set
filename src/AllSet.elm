module AllSet
    ( Set
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , foldl, foldr, map
    , filter, partition
    , union, intersect, diff
    , toList, fromList
    ) where

{-| A set of unique values. The values can be any a type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of a types.

Insert, remove, and query operations all take *O(log n)* time. Set equality with
`(==)` is unreliable and should not be used.

# Sets
@docs Set

# Build
@docs empty, singleton, insert, remove

# Query
@docs isEmpty, member, size

# Combine
@docs union, intersect, diff

# Lists
@docs toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

-}

import Basics exposing ((<|))
import MyDict as MyDict
import List as List


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type Set t =
  Set_elm_builtin (MyDict.MyDict t ())

dict = MyDict.makeDict toString

{-| Create an empty set.
-}
empty : Set a
empty =
  Set_elm_builtin dict.empty


{-| Create a set with one value.
-}
singleton : a -> Set a
singleton k =
  Set_elm_builtin <| dict.singleton k ()


{-| Insert a value into a set.
-}
insert : a -> Set a -> Set a
insert k (Set_elm_builtin d) =
  Set_elm_builtin <| dict.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> Set a -> Set a
remove k (Set_elm_builtin d) =
  Set_elm_builtin <| dict.remove k d


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty (Set_elm_builtin d) =
  dict.isEmpty d


{-| Determine if a value is in a set.
-}
member : a -> Set a -> Bool
member k (Set_elm_builtin d) =
  dict.member k d


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (Set_elm_builtin d) =
  dict.size d


{-| Get the union of two sets. Keep all values.
-}
union : Set a -> Set a -> Set a
union (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| dict.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set a -> Set a -> Set a
intersect (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| dict.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set a -> Set a -> Set a
diff (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| dict.diff d1 d2


{-| Convert a set into a list.
-}
toList : Set a -> List a
toList (Set_elm_builtin d) =
  dict.keys d


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> Set a
fromList xs = List.foldl insert empty xs


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl f b (Set_elm_builtin d) =
  dict.foldl (\k _ b -> f k b) b d


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr f b (Set_elm_builtin d) =
  dict.foldr (\k _ b -> f k b) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> a') -> Set a -> Set a'
map f s = fromList (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> Set a -> Set a
filter p (Set_elm_builtin d) =
  Set_elm_builtin <| dict.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> Set a -> (Set a, Set a)
partition p (Set_elm_builtin d) =
  let
    (p1, p2) = dict.partition (\k _ -> p k) d
  in
    (Set_elm_builtin p1, Set_elm_builtin p2)
