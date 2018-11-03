# aeson-generic-ts

Convert Haskell to Typescript in a highly configurable way with Generics 

## Table of Contents
  - [Quick Start](#quick-start)
  - [About](#begin)
  - [Design Goals](#design-goals)
  - [Full Example](#full-example)
  - [Flavors](#flavors)
    - [Built In](#built-in-flavors)
      - [Vanilla](#vanilla)
      - [Fp-Ts](#FpTs)
  - [Roadmap](#roadmap)
  
## Quick Start

1. First Derive a generic Instance of a type:

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}

import           GHC.Generics
import           Data.Text          (Text)

data User =
  User 
    {name :: Text
    ,age  :: Int
    } deriving (Generic, BridgeType)
  
```

2. Pick a [flavor](#flavors) you'd like to generate the TS with, and run the print function

```haskell

-- Note here we are using the flavor Vanilla

printUser :: IO ()
printUser =
    printFromBridge (Proxy :: Proxy Vanilla) (Proxy :: Proxy ComplexRecord)
```

This prints the following:

```typescript
interface User {
  name : string
  age  : number
}
```

## About

This project is under development and will be used in production once it's ready. I'd call it's current state alpha, as there are a lot of design decisions still being made about this library. See Design Goals for what's going into the implementation of this library

## Design Goals 

Typescript has many ways of doing the same thing, and there are lots of opinions on how to do this. For example, product types can be represented by an interface or (immutable) classes. For this reason, configurability is considered a primary design goal. Here are all of them

1. Ability to customize how Haskell types are represented as TS types

2. Prebaked configurations for the most common ways people like to represent TS types

3. The use of well known libraries as configuration options ([built-in flavors](#built-in-flavors)). Two examples I will provide default implementations for are [fp-ts](https://github.com/gcanti/fp-ts) and [unionize](https://github.com/pelotom/unionize)

4. A simple interface for providing your own custom translation

Achieving high configurability relies on using Generics to first translate Haskell types to an intermediate bridge language. A configuration data structure can then be passed into a translation function to achieve nearly any typescript representation you desire.

## Flavors

A flavor is just another name for a type that represents how you want your Typescript customized to. There are currently two supported flavors: **Vanilla** and **FpTS**. You can also write your own flavors (TODO...instructions for this)

### Built In Flavors

We will use the following example type to see how it varies across flavors

```haskell
newtype AnOption = AnOption (Maybe Text) deriving (Generic, BridgeType)
```

#### Vanilla

```haskell
printAnOption =
    ts <- printFromBridge (Proxy :: Proxy Vanilla) (Proxy :: Proxy AnOption)
    
# > type AnOption = null | string
```

#### FpTs
```haskell
printAnOption =
    ts <- printFromBridge (Proxy :: Proxy FpTs) (Proxy :: Proxy AnOption)
    
# type AnOption = Option<string>
```

### Defining your own flavors

TODO.....



## Full Example

The best place for up to date examples is probably just to look at test, but here's a basic one

Given these haskell types:

```haskell
import Data.Text
import GHC.Generics

data ComplexRecord =
  ComplexRecord
    {anIntField    :: Int
    ,aTextField    :: Text
    ,aUnion        :: SampleUnion
    ,aMaybeType    :: Maybe Text
    ,aSimpleRecord :: SimpleRecord
    } deriving (Generic, BridgeType)

data SimpleUnTagged = F Int deriving (Generic, BridgeType)

data SampleUnion = FirstCon Int | SecondCon Text deriving (Generic, BridgeType)
```

Specify a flavor to print to TS. Here's an example using the Vanilla Flavor

```
printComplexRecord :: IO ()
printComplexRecord =
    printFromBridge (Proxy :: Proxy Vanilla) (Proxy :: Proxy ComplexRecord)
```

Generates the following typescript types

```typescript

interface ComplexRecord {
  anIntField : number
  aTextField : string
  aUnion : SampleUnion
  aMaybeType : string | null
  aSimpleRecord : SimpleRecord
}
```


## Roadmap

1. More complete FP-TS functionality
2. Unionize library flavor
3. Remove intermediate bridge language
4. Figure out a cleaner ADT interface for customizing TS
5. I dunno, lots of stuff probably. Make it more production ready I guess
