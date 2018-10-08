# aeson-generic-ts

Convert Haskell to Typescript in a highly configurable way with Generics 

## About

This project is under pretty heavy development and will be used in production once it's ready. I'd call it's current state alpha, as there are a lot of design decisions still being made about this library. See Design Goals for what's going into the implementation of this library

## Design Goals 

Typescript has many ways of doing the same thing, and there are lots of opinions on how to do this. For example, product types can be represented by an interface or (immutable) classes. For this reason, configurability is considered a primary design goal. Here are all of them

1. Ability to customize how Haskell types are represented as TS types

2. Prebaked configurations for the most common ways people like to represent TS types

3. The use of well known libraries as configuration options. Two examples I will provide default implementations for are [fp-ts][https://github.com/gcanti/fp-ts] and [unionize](https://github.com/pelotom/unionize)

4. A simple interface for providing your own custom translation

Achieving high configurability relies on using Generics to first translate Haskell types to an intermediate bridge language. A configuration data structure can then be passed into a translation function to achieve nearly any typescript representation you desire.

## Example

Given this haskell type:

```haskell
data User = User
    { name         :: PersonName
    , age          :: Int
    , placesLived  :: [Text]
    , isRegistered :: Bool
    } deriving (Generic, TypescriptType)

data PersonName =
  PersonName
   { firstName     :: Text
   , middleInitial :: Maybe Text
   , lastName      :: Text
   } deriving (Generic, TypescriptType)

```

Generates the following typescript types

```typescript

interface User {
   name : PersonName
   age : number
   placesLived : Array<string>
   isRegistered : boolean
}

interface PersonName {
   firstName : string
   middleInitial : string
   lastName : string
}
```

### Roadmap

- [x] Basic encoding of record to interface types
- [ ] Sum type -> tagged union
- [ ] Typescript formatting options (e.g. interfaces vs classes)
- [ ] Full coverage of all likely scenarios

