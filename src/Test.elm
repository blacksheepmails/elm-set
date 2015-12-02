import EverySet
import AllSet
import List
import Graphics.Element exposing (show)

a = EverySet.fromList([1,2])
b = EverySet.fromList([1,2])
c = EverySet.fromList([1..10000])
d = List.foldr (\x set -> EverySet.insert x set) EverySet.empty [5000..20000]
main = show <| EverySet.fromList([a,b,c,d])

--a = AllSet.fromList([1,2])
--b = AllSet.fromList([1,2])
--c = AllSet.fromList([1..10000])
--d = List.foldr (\x set -> AllSet.insert x set) AllSet.empty [5000..20000]
--main = show <| AllSet.fromList([a,b,c,d])