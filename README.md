# aeson-generic-ts

Encoding of Haskell types to generic typescript types

Given this haskell type:

```
data User =
  User
    {name         :: Text
    ,age          :: Int
    } deriving (Generic, TypescriptType)
```

Becomes this typescript type

```
interface User {
   name : string
   age : number
}
```
