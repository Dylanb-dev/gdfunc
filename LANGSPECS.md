# GDFunc Language Specification

**Version 1.0 - Linear-by-Default Functional Language**

## Table of Contents

1. [Introduction](#introduction)
2. [Core Principles](#core-principles)
3. [Type System](#type-system)
4. [Syntax](#syntax)
5. [Linear Semantics](#linear-semantics)
6. [Borrowing](#borrowing)
7. [Shared Types](#shared-types)
8. [Pattern Matching](#pattern-matching)
9. [Functions](#functions)
10. [Control Flow](#control-flow)
11. [Standard Library](#standard-library)
12. [Examples](#examples)
13. [Comparison with Other Languages](#comparison-with-other-languages)

---

## Introduction

GDFunc is a functional programming language with **linear types by default**. Every value must be used exactly once unless explicitly borrowed or marked as shared. This design provides:

- **Memory safety** without garbage collection
- **Resource safety** (files, sockets, locks) at compile time
- **Zero-cost abstractions** for ownership
- **Clear data flow** through type signatures

### Philosophy

> "Make the safe thing easy and the unsafe thing explicit."

In GDFunc, consuming values is the default. Aliasing and copying require explicit syntax.

---

## Core Principles

### 1. Linear by Default

Every value is **linear** (must be used exactly once) unless stated otherwise:

```elm
-- This function consumes its argument
increment : Int -> Int
increment x = x + 1

-- After calling increment, you cannot use the original value
let y = increment x
-- x is now consumed and cannot be used
```

### 2. Explicit Borrowing

Use `&` to borrow a value without consuming it:

```elm
-- This function borrows its argument (read-only access)
length : &List a -> Int
length list =
    case list of
        [] -> 0
        _ :: rest -> 1 + length rest

-- After calling length, you can still use the list
let len = length &myList
let doubled = map (\x -> x * 2) myList  -- myList still available
```

### 3. Opt-in Sharing

Use `shared` for types that can be freely copied:

```elm
-- Shared types can be used multiple times
type shared Config = Config String Int

useConfigTwice : &Config -> (String, Int)
useConfigTwice config =
    case config of
        Config name value ->
            (name ++ name, value + value)  -- 'name' and 'value' used twice
```

---

## Type System

### Basic Types

All basic types are **shared by default** (can be freely copied):

```elm
Int      -- shared
Float    -- shared
String   -- shared
Bool     -- shared
Char     -- shared
```

### Linear Types

User-defined types are **linear by default**:

```elm
-- Linear type - must be consumed exactly once
type FileHandle = FileHandle String

-- Linear type with type parameter
type List a
    = Empty
    | Cons a (List a)

-- Linear type with multiple constructors
type Result error value
    = Ok value
    | Err error
```

### Shared Types

Mark types as `shared` to allow unrestricted use:

```elm
-- This type can be copied freely
type shared Point = Point Float Float

-- Shared types with parameters
type shared Maybe a = Nothing | Just a

-- Shared record
type shared User = User
    { name : String
    , age : Int
    , email : String
    }
```

### Type Annotations

```elm
-- Linear type
x : List Int

-- Borrowed type
x : &List Int

-- Shared type
x : shared List Int
-- OR (if List is declared as shared)
x : List Int
```

---

## Syntax

### Module Declaration

```elm
module MyModule exposing (function1, Type1, function2)
```

### Type Declarations

```elm
-- Linear type (default)
type Stack a = Stack (List a)

-- Shared type
type shared Coordinate = Coordinate Int Int

-- Type alias
type alias Name = String

-- Shared type alias
type shared alias Config = { host : String, port : Int }
```

### Function Declarations

```elm
-- Function consuming its argument
push : a -> Stack a -> Stack a
push item stack =
    case stack of
        Stack items -> Stack (item :: items)

-- Function borrowing its argument
isEmpty : &Stack a -> Bool
isEmpty stack =
    case stack of
        Stack [] -> True
        Stack _ -> False

-- Function with borrowed function parameter
map : (&a -> b) -> List a -> List b
map f list =
    case list of
        [] -> []
        x :: xs -> f x :: map f xs
```

### Let Bindings

```elm
-- Linear binding - value consumed in expression
let x = createValue
in useValue x

-- Multiple bindings
let
    x = value1
    y = value2
in
    combine x y

-- Pattern matching in let
let (a, b) = pair
in a + b
```

### Comments

```elm
-- Single line comment

{-
   Multi-line comment
   Can be nested {- like this -}
-}

-- Documentation comment (appears before function)
{-| Pushes an item onto the stack.
    Consumes both the item and the stack.
-}
push : a -> Stack a -> Stack a
```

---

## Linear Semantics

### The Use-Exactly-Once Rule

Every linear value must be used **exactly once** in its scope:

```elm
-- ✅ VALID - x used exactly once
let x = getValue
in consume x

-- ❌ INVALID - x used twice
let x = getValue
in (consume x, consume x)

-- ❌ INVALID - x never used
let x = getValue
in 42

-- ✅ VALID - both branches consume x
let x = getValue
in if condition then
       consume x
   else
       transform x
```

### Consuming vs Borrowing

```elm
-- Consuming a value transfers ownership
takeOwnership : List a -> Int
takeOwnership list =
    case list of
        [] -> 0
        x :: xs -> 1 + takeOwnership xs
-- 'list' is consumed, cannot be used again


-- Borrowing a value allows inspection without consuming
peek : &List a -> Maybe a
peek list =
    case list of
        [] -> Nothing
        x :: _ -> Just x
-- 'list' can still be used after this call
```

### Linear Values in Data Structures

When a linear value is placed in a data structure, the entire structure becomes linear:

```elm
-- If FileHandle is linear, then this tuple is linear
type Pair = Pair FileHandle String

-- You must consume the entire pair to get the FileHandle
processPair : Pair -> String
processPair pair =
    case pair of
        Pair handle name ->
            let content = readFile handle
            in content ++ name
```

---

## Borrowing

### Borrow Operator `&`

The `&` operator creates a **non-consuming reference** to a value:

```elm
-- Function that borrows its argument
printLength : &List a -> ()
printLength list =
    print (length list)

-- Usage
let myList = [1, 2, 3]
let _ = printLength &myList  -- borrow myList
let doubled = map (\x -> x * 2) myList  -- myList still available
```

### Borrowing Rules

1. **Multiple borrows allowed**: You can borrow the same value multiple times
2. **No mutation**: Borrowed values are read-only
3. **Cannot consume borrowed values**: `&a` cannot be converted to `a`
4. **Automatic dereferencing**: When pattern matching, borrows are transparent

```elm
-- Multiple borrows are fine
let list = [1, 2, 3]
let len1 = length &list
let len2 = length &list
let len3 = length &list
-- Finally consume the list
let result = sum list

-- Borrowed pattern matching
head : &List a -> Maybe a
head list =
    case list of
        [] -> Nothing
        x :: _ -> Just x  -- x is copied (if shared) or borrowed
```

### When to Use Borrowing

Use borrowing when you need to:

- **Inspect** a value without consuming it
- **Query** a data structure
- **Call multiple functions** on the same value
- **Pass configuration** that shouldn't be consumed

```elm
-- Good use of borrowing - configuration
processWithConfig : &Config -> Data -> Result
processWithConfig config data =
    let validated = validate config data
        transformed = transform config validated
    in finalize config transformed

-- Good use of borrowing - query
isEmpty : &List a -> Bool
size : &List a -> Int
contains : &a -> &List a -> Bool
```

---

## Shared Types

### Declaring Shared Types

Types marked `shared` can be copied freely:

```elm
type shared Point = Point Float Float
type shared Color = Red | Green | Blue
type shared Config = Config { timeout : Int, retries : Int }
```

### Shared Semantics

Shared values do **not** follow linear rules:

```elm
-- Can use shared values multiple times
let point = Point 3.0 4.0
let x1 = getX point
let x2 = getX point  -- point still available
let dist = distance point origin  -- still available

-- Shared values in functions
duplicate : shared a -> (a, a)
duplicate x = (x, x)  -- x used twice - OK because shared
```

### When to Use Shared

Use `shared` for:

- **Primitive-like types** (Points, Colors, Coordinates)
- **Configuration data** used throughout the program
- **Small, immutable data** that's cheaper to copy than manage linearly
- **Constants and enums**

```elm
-- Good candidates for shared
type shared HttpMethod = GET | POST | PUT | DELETE
type shared Coordinate = Coordinate Int Int
type shared RGB = RGB Int Int Int

-- Bad candidates for shared (should be linear)
type FileHandle = FileHandle String  -- resource
type Connection = Connection Socket  -- resource
type LargeDataset = LargeDataset (Array Float)  -- expensive to copy
```

### Shared vs Linear Trade-offs

| Aspect      | Linear                               | Shared                     |
| ----------- | ------------------------------------ | -------------------------- |
| Memory      | Single owner, no copying             | May copy frequently        |
| Safety      | Prevents double-free, use-after-free | No ownership guarantees    |
| Ergonomics  | Requires explicit threading          | Use anywhere freely        |
| Resources   | Perfect for files, sockets, locks    | Cannot represent resources |
| Performance | Zero-cost, predictable               | Potential copying overhead |

---

## Pattern Matching

### Basic Pattern Matching

```elm
-- Pattern matching consumes the matched value
case list of
    [] -> 0
    x :: xs -> 1 + length xs
-- 'list' is consumed by the case expression
```

### Pattern Matching with Borrowing

```elm
-- Borrowing in pattern match
peek : &List a -> Maybe a
peek list =
    case list of
        [] -> Nothing
        x :: _ -> Just x
-- 'list' is not consumed, but we get a copy/borrow of 'x'
```

### Nested Patterns

```elm
-- All variables in pattern must be consumed
case pair of
    (x, y) -> consume x y  -- both x and y used

-- Nested linear values
case listOfLists of
    [] -> Empty
    innerList :: rest ->
        process innerList (combine rest)
```

### Wildcard Pattern

Use `_` to explicitly discard a value:

```elm
-- Discarding with _
case result of
    Ok value -> value
    Err _ -> defaultValue  -- error is discarded

-- Discarding tuple elements
case triple of
    (x, _, z) -> x + z  -- middle element discarded
```

### Guards

```elm
-- Pattern matching with guards
case number of
    x | x < 0 -> "negative"
    x | x == 0 -> "zero"
    x -> "positive"
```

---

## Functions

### Function Types

```elm
-- Consumes argument
increment : Int -> Int

-- Borrows argument
double : &Int -> Int

-- Multiple arguments (all consumed by default)
add : Int -> Int -> Int

-- Mixed borrowing and consuming
insertWithConfig : &Config -> a -> List a -> List a

-- Shared return type
getConfig : FilePath -> shared Config
```

### Function Definitions

```elm
-- Simple function
square : Int -> Int
square x = x * x

-- Multi-argument function
combine : String -> String -> String
combine a b = a ++ b

-- Function with where clause
distance : Point -> Point -> Float
distance p1 p2 =
    sqrt (dx * dx + dy * dy)
    where
        dx = p1.x - p2.x
        dy = p1.y - p2.y

-- Function with case expression
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    case denominator of
        0 -> Nothing
        d -> Just (numerator / d)
```

### Lambda Functions

```elm
-- Lambda consuming its argument
\x -> x + 1

-- Lambda borrowing its argument
\&x -> x * 2

-- Multiple arguments
\x y -> x + y

-- Pattern matching lambda
\(x, y) -> x + y

-- Lambda with case
\list -> case list of
    [] -> 0
    _ :: rest -> 1 + length rest
```

### Higher-Order Functions

```elm
-- Function taking borrowed function
map : (&a -> b) -> List a -> List b
map f list =
    case list of
        [] -> []
        x :: xs -> f x :: map f xs

-- Function taking consuming function
transform : (a -> b) -> List a -> List b
transform f list =
    case list of
        [] -> []
        x :: xs -> f x :: transform f xs

-- Returning functions
add : Int -> (Int -> Int)
add x = \y -> x + y

-- Closure over linear value
makeAdder : Int -> (Int -> Int)
makeAdder x =
    let addX = \y -> x + y  -- x is captured (must be shared)
    in addX
```

---

## Control Flow

### If-Then-Else

Both branches must consume the same linear values:

```elm
-- ✅ VALID - both branches consume x
let result = if condition then
                consume x
             else
                transform x

-- ❌ INVALID - only one branch consumes x
let result = if condition then
                consume x
             else
                42
```

### Case Expressions

All patterns must consume bound variables:

```elm
-- ✅ VALID - all branches consume list
case list of
    [] -> Empty
    x :: xs -> process x xs

-- ❌ INVALID - second branch doesn't consume xs
case list of
    [] -> Empty
    x :: xs -> process x  -- xs not used!
```

### Let Expressions

```elm
-- Simple let
let x = getValue
in useValue x

-- Multiple bindings
let
    x = value1
    y = value2
    z = combine x y
in
    process z

-- Nested lets
let x = outer
in let y = inner x
   in combine y
```

---

## Standard Library

### List Module

```elm
module List exposing (..)

-- Linear list operations

-- Consuming operations
map : (&a -> b) -> List a -> List b
filter : (&a -> Bool) -> List a -> List a
fold : (&b -> a -> b) -> b -> List a -> b
append : List a -> List a -> List a
reverse : List a -> List a
take : Int -> List a -> List a
drop : Int -> List a -> List a

-- Borrowing operations (queries)
length : &List a -> Int
isEmpty : &List a -> Bool
head : &List a -> Maybe a
tail : &List a -> Maybe (List a)  -- returns new linear list
member : &a -> &List a -> Bool

-- Hybrid operations
partition : (&a -> Bool) -> List a -> (List a, List a)
unzip : List (a, b) -> (List a, List b)
```

### Maybe Module

```elm
module Maybe exposing (..)

type shared Maybe a = Nothing | Just a

map : (&a -> b) -> Maybe a -> Maybe b
withDefault : a -> Maybe a -> a
andThen : (&a -> Maybe b) -> Maybe a -> Maybe b
```

### Result Module

```elm
module Result exposing (..)

type Result error value = Ok value | Err error

map : (&a -> b) -> Result e a -> Result e b
mapError : (&e1 -> e2) -> Result e1 a -> Result e2 a
andThen : (&a -> Result e b) -> Result e a -> Result e b
withDefault : a -> Result e a -> a
```

### String Module

```elm
module String exposing (..)

-- Strings are shared by default

length : String -> Int  -- Can use String multiple times
append : String -> String -> String
concat : List String -> String
split : String -> String -> List String
trim : String -> String
toUpper : String -> String
toLower : String -> String
```

### File Module (Example of Resource Management)

```elm
module File exposing (..)

type FileHandle = FileHandle String

-- File operations consume the handle
open : FilePath -> Result Error FileHandle
read : FileHandle -> Result Error (String, FileHandle)
write : String -> FileHandle -> Result Error FileHandle
close : FileHandle -> Result Error ()

-- Safe file reading pattern
readEntireFile : FilePath -> Result Error String
readEntireFile path =
    case open path of
        Err e -> Err e
        Ok handle ->
            case read handle of
                Err e -> Err e
                Ok (content, handle2) ->
                    case close handle2 of
                        Err e -> Err e
                        Ok () -> Ok content
```

---

## Examples

### Example 1: Basic List Operations

```elm
module ListExample exposing (..)

-- Sum a list (consuming it)
sum : List Int -> Int
sum list =
    case list of
        [] -> 0
        x :: xs -> x + sum xs

-- Check if list is empty (borrowing)
isEmpty : &List a -> Bool
isEmpty list =
    case list of
        [] -> True
        _ -> False

-- Usage
main =
    let numbers = [1, 2, 3, 4, 5]
        empty = isEmpty &numbers  -- borrow
        total = sum numbers  -- consume
    in (empty, total)  -- (False, 15)
```

### Example 2: File Handling

```elm
module FileExample exposing (..)

type FileHandle = FileHandle String

open : String -> FileHandle
read : FileHandle -> (String, FileHandle)
close : FileHandle -> ()

-- Safe pattern: must close file
readFile : String -> String
readFile path =
    let handle1 = open path
        (content, handle2) = read handle1  -- handle1 consumed
        _ = close handle2  -- handle2 consumed
    in content

-- ❌ This won't compile - handle leak!
readFileBad : String -> String
readFileBad path =
    let handle = open path
        (content, _) = read handle
    in content  -- ERROR: second handle not closed!
```

### Example 3: Builder Pattern

```elm
module BuilderExample exposing (..)

type QueryBuilder = QueryBuilder
    { table : Maybe String
    , conditions : List String
    , limit : Maybe Int
    }

new : QueryBuilder
new = QueryBuilder { table = Nothing, conditions = [], limit = Nothing }

from : String -> QueryBuilder -> QueryBuilder
from tableName builder =
    case builder of
        QueryBuilder state ->
            QueryBuilder { state | table = Just tableName }

where_ : String -> QueryBuilder -> QueryBuilder
where_ condition builder =
    case builder of
        QueryBuilder state ->
            QueryBuilder { state | conditions = condition :: state.conditions }

limit : Int -> QueryBuilder -> QueryBuilder
limit n builder =
    case builder of
        QueryBuilder state ->
            QueryBuilder { state | limit = Just n }

build : QueryBuilder -> String
build builder =
    case builder of
        QueryBuilder state ->
            "SELECT * FROM " ++ Maybe.withDefault "unknown" state.table
            ++ buildConditions state.conditions
            ++ buildLimit state.limit

buildConditions : List String -> String
buildConditions conds =
    case conds of
        [] -> ""
        _ -> " WHERE " ++ String.join " AND " conds

buildLimit : Maybe Int -> String
buildLimit maybeLimit =
    case maybeLimit of
        Nothing -> ""
        Just n -> " LIMIT " ++ String.fromInt n

-- Usage - chaining works because each function returns new builder
query : String
query =
    new
        |> from "users"
        |> where_ "age > 18"
        |> where_ "active = true"
        |> limit 10
        |> build
```

### Example 4: Resource Pool

```elm
module PoolExample exposing (..)

type Pool a = Pool (List a)

create : List a -> Pool a
create items = Pool items

acquire : Pool a -> (Maybe a, Pool a)
acquire pool =
    case pool of
        Pool [] -> (Nothing, Pool [])
        Pool (item :: rest) -> (Just item, Pool rest)

release : a -> Pool a -> Pool a
release item pool =
    case pool of
        Pool items -> Pool (item :: items)

-- Using the pool safely
withResource : Pool a -> (a -> b) -> (Maybe b, Pool a)
withResource pool use =
    case acquire pool of
        (Nothing, pool2) -> (Nothing, pool2)
        (Just resource, pool2) ->
            let result = use resource
                pool3 = release resource pool2
            in (Just result, pool3)

-- Example usage
main =
    let pool1 = create ["conn1", "conn2", "conn3"]
        (result1, pool2) = withResource pool1 (\conn -> "Used " ++ conn)
        (result2, pool3) = withResource pool2 (\conn -> "Used " ++ conn)
    in (result1, result2, pool3)
```

### Example 5: State Machine

```elm
module StateMachine exposing (..)

type State
    = Idle
    | Running Int
    | Paused Int
    | Stopped

start : State -> State
start state =
    case state of
        Idle -> Running 0
        _ -> state  -- Other states unchanged

tick : State -> State
tick state =
    case state of
        Running n -> Running (n + 1)
        _ -> state

pause : State -> State
pause state =
    case state of
        Running n -> Paused n
        _ -> state

resume : State -> State
resume state =
    case state of
        Paused n -> Running n
        _ -> state

stop : State -> State
stop state = Stopped

-- Usage - state is threaded through
runMachine : State
runMachine =
    Idle
        |> start
        |> tick
        |> tick
        |> tick
        |> pause
        |> resume
        |> tick
        |> stop
```

### Example 6: Parsing with Linear Input

```elm
module Parser exposing (..)

type Input = Input String Int  -- (string, position)

type ParseResult a =
    | Success a Input  -- (result, remaining input)
    | Failure String

char : Char -> Input -> ParseResult Char
char expected input =
    case input of
        Input str pos ->
            case String.slice pos (pos + 1) str of
                "" -> Failure "End of input"
                c ->
                    if c == String.fromChar expected then
                        Success expected (Input str (pos + 1))
                    else
                        Failure ("Expected " ++ String.fromChar expected)

string : String -> Input -> ParseResult String
string expected input =
    case input of
        Input str pos ->
            let len = String.length expected
                actual = String.slice pos (pos + len) str
            in
            if actual == expected then
                Success expected (Input str (pos + len))
            else
                Failure ("Expected " ++ expected)

-- Combinator: try first parser, if it fails, try second
or : (Input -> ParseResult a) -> (Input -> ParseResult a) -> Input -> ParseResult a
or parser1 parser2 input =
    case parser1 input of
        Success result remaining -> Success result remaining
        Failure _ -> parser2 input

-- Usage
parseHello : Input -> ParseResult String
parseHello = string "Hello"

parseWorld : Input -> ParseResult String
parseWorld = string "World"

parseGreeting : Input -> ParseResult String
parseGreeting = or parseHello parseWorld
```

### Example 7: Shared Configuration

```elm
module ConfigExample exposing (..)

type shared Config = Config
    { apiKey : String
    , timeout : Int
    , retries : Int
    , debug : Bool
    }

-- Config is shared, so it can be borrowed everywhere
makeRequest : &Config -> String -> Result Error Response
makeRequest config url =
    if config.debug then
        log ("Making request to " ++ url)
    else
        ()
    -- ... make request with config.apiKey, config.timeout, etc

processData : &Config -> Data -> Result Error ProcessedData
processData config data =
    if config.debug then
        log "Processing data"
    else
        ()
    -- ... process with config settings

-- Can use config multiple times without consuming it
main : Config -> Result Error Response
main config =
    let data = fetchData &config
        processed = processData &config data
        result = saveResult &config processed
    in result
```

---

## Comparison with Other Languages

### vs Elm (Linear-by-Default vs Unrestricted-by-Default)

**Elm:**

```elm
-- Everything is immutable and can be used multiple times
list = [1, 2, 3]
len = List.length list
doubled = List.map (\x -> x * 2) list  -- list still available
tripled = List.map (\x -> x * 3) list  -- list still available
```

**GDFunc:**

```elm
-- Values consumed by default
list = [1, 2, 3]
len = length &list  -- borrow to not consume
doubled = map (\x -> x * 2) list  -- list consumed here
-- tripled = map (\x -> x * 3) list  -- ERROR: list already consumed
```

### vs Rust (Ownership)

**Rust:**

```rust
// Ownership by default, explicit borrowing
let list = vec![1, 2, 3];
let len = list.len();  // auto-borrow
let doubled: Vec<_> = list.into_iter().map(|x| x * 2).collect();  // consumed
// let tripled = list.iter();  // ERROR: list moved
```

**GDFunc:**

```elm
-- Similar ownership model, but functional
list = [1, 2, 3]
len = length &list  -- explicit borrow
doubled = map (\x -> x * 2) list  -- explicit consume
```

### vs Haskell (Lazy vs Strict + Linear)

**Haskell:**

```haskell
-- Lazy, unrestricted (with Linear Haskell extension for linearity)
list = [1, 2, 3]
len = length list
doubled = map (*2) list  -- list still available due to laziness/sharing
```

**GDFunc:**

```elm
-- Strict evaluation, explicit linearity
list = [1, 2, 3]
len = length &list  -- must borrow
doubled = map (\x -> x * 2) list  -- explicit consumption
```

---

## Appendix: Grammar Summary

```
module -> "module" ModuleName "exposing" "(" exports ")"

type -> "type" TypeName params "=" constructors
      | "type" "shared" TypeName params "=" constructors

function -> name ":" type
            name params "=" expr

expr -> literal
      | variable
      | "(" expr ")"
      | expr expr
      | "\\" pattern "->" expr
      | "let" bindings "in" expr
      | "case" expr "of" branches
      | "if" expr "then" expr "else" expr

pattern -> variable
         | "_"
         | constructor patterns
         | "(" pattern "," pattern ")"
         | pattern "::" pattern

type_annotation -> type_name
                 | "&" type_annotation  -- borrowed
                 | "shared" type_annotation
                 | type_annotation "->" type_annotation
                 | "(" type_annotation ")"
```

---

## Conclusion

GDFunc provides **safety through linearity** while maintaining the **elegance of functional programming**. By making linearity the default, the language:

- Prevents resource leaks at compile time
- Makes data flow explicit
- Eliminates whole classes of bugs
- Provides zero-cost abstractions

The trade-off is more explicit code compared to unrestricted functional languages, but the safety guarantees and performance benefits make this worthwhile for systems programming and resource-critical applications.

For more examples and advanced patterns, see the [GDFunc Cookbook](cookbook.md).

---

**End of Specification**
