module Lang.Infer.NameDict exposing
    ( NameDict
    , diff
    , empty
    , filter
    , foldl
    , foldr
    , fromList
    , get
    , insert
    , intersect
    , isEmpty
    , keys
    , map
    , member
    , merge
    , partition
    , remove
    , singleton
    , size
    , toList
    , union
    , update
    , values
    )

import Ex.ExDict as ExDict exposing (ExDict)
import Lang.Canonical.Name as Name exposing (Name)


type alias NameDict value =
    ExDict Name value


deps : { compare : Name -> Name -> Order }
deps =
    { compare = Name.compare }


empty : NameDict v
empty =
    ExDict.empty


singleton : Name -> v -> NameDict v
singleton =
    ExDict.singleton


insert : Name -> v -> NameDict v -> NameDict v
insert =
    ExDict.insert deps


update : Name -> (Maybe v -> Maybe v) -> NameDict v -> NameDict v
update =
    ExDict.update deps


remove : Name -> NameDict v -> NameDict v
remove =
    ExDict.remove deps


isEmpty : NameDict v -> Bool
isEmpty =
    ExDict.isEmpty


member : Name -> NameDict v -> Bool
member =
    ExDict.member deps


get : Name -> NameDict v -> Maybe v
get =
    ExDict.get deps


size : NameDict v -> Int
size =
    ExDict.size


keys : NameDict v -> List Name
keys =
    ExDict.keys


values : NameDict v -> List v
values =
    ExDict.values


toList : NameDict v -> List ( Name, v )
toList =
    ExDict.toList


fromList : List ( Name, v ) -> NameDict v
fromList =
    ExDict.fromList deps


map : (Name -> a -> b) -> NameDict a -> NameDict b
map =
    ExDict.map


foldl : (Name -> v -> b -> b) -> b -> NameDict v -> b
foldl =
    ExDict.foldl


foldr : (Name -> v -> b -> b) -> b -> NameDict v -> b
foldr =
    ExDict.foldr


filter : (Name -> v -> Bool) -> NameDict v -> NameDict v
filter =
    ExDict.filter deps


partition : (Name -> v -> Bool) -> NameDict v -> ( NameDict v, NameDict v )
partition =
    ExDict.partition deps


union : NameDict v -> NameDict v -> NameDict v
union =
    ExDict.union deps


intersect : NameDict v -> NameDict v -> NameDict v
intersect =
    ExDict.intersect deps


diff : NameDict a -> NameDict b -> NameDict a
diff =
    ExDict.diff deps


merge : (Name -> a -> result -> result) -> (Name -> a -> b -> result -> result) -> (Name -> b -> result -> result) -> NameDict a -> NameDict b -> result -> result
merge =
    ExDict.merge deps
