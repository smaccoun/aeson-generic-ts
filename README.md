# aeson-generic-ts

Encoding of Haskell types to generic typescript types

Given this haskell type:

```
data User = User
    {name :: PersonName
    ,age  :: Int
    } deriving (Generic, TypescriptType)

data PersonName =
  PersonName
   {firstName     :: Text
   ,middleInitial :: Maybe Text
   ,lastName      :: Text
   } deriving (Generic, TypescriptType)

```

Generates the following typescript types

```
interface User {
   name : PersonName
   age : number
}
interface PersonName {
   firstName : string
   middleInitial : string | null
   lastName : string
}
```
