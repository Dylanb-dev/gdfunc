Standard Elm Quicksort

```elm
elmmodule Quicksort exposing (quicksort)

quicksort : List comparable -> List comparable
quicksort list =
case list of
[] ->
[]

        pivot :: rest ->
            let
                smaller =
                    List.filter (\x -> x <= pivot) rest

                larger =
                    List.filter (\x -> x > pivot) rest
            in
            quicksort smaller ++ [pivot] ++ quicksort larger
```

GDFunc Quicksort

```elm
module Quicksort exposing (quicksort)

-- Linear lists use ! to indicate they must be consumed exactly once
quicksort : List! comparable -> List! comparable
quicksort list =
    case! list of
        -- Pattern matching on linear values uses case!
        [] ->
            []

        pivot :: rest ->
            let
                -- partition consumes rest linearly and returns two new linear lists
                (smaller!, larger!) =
                    partition (\x -> x <= pivot) rest!
            in
            -- concat3 consumes all three linear lists
            concat3 (quicksort smaller!) [pivot] (quicksort larger!)


-- Helper function that partitions while respecting linearity
partition : (a -> Bool) -> List! a -> (List! a, List! a)
partition predicate list! =
    case! list! of
        [] ->
            ([], [])

        x :: xs! ->
            let
                (trues!, falses!) = partition predicate xs!
            in
            if predicate x then
                (x :: trues!, falses!)
            else
                (trues!, x :: falses!)


-- Concatenate three lists, consuming all linearly
concat3 : List! a -> List a -> List! a -> List! a
concat3 left! middle right! =
    append! left! (append middle right!)


-- Linear append that consumes its first argument
append! : List! a -> List! a -> List! a
append! xs! ys! =
    case! xs! of
        [] ->
            ys!

        x :: rest! ->
            x :: append! rest! ys!


-- Regular append for non-linear lists
append : List a -> List! a -> List! a
append xs ys! =
    case xs of
        [] ->
            ys!

        x :: rest ->
            x :: append rest ys!
```

### Key Differences in GDFunc:

Linear type annotation: `List!` a means the list must be used exactly once
Linear pattern matching: `case!` is used to destructure linear values
Explicit consumption: Variables with `!` must be consumed in their scope
Dual type system: Both linear (`List!`) and unrestricted (`List`) types coexist
No implicit copying: You can't use `List.filter` twice on the same linear list

Design Philosophy:
Type signatures show ownership:

`List a` - can be used multiple times (unrestricted)
`List! a` - must be used exactly once (linear)

Benefits:

- Prevents double-free and use-after-free at compile time
- Makes resource management explicit
- Zero-cost abstractions for memory management
- Clear ownership semantics

Trade-offs:

- More verbose (need explicit partition instead of filtering twice)
- Steeper learning curve
- Less convenient for immutable-by-default style
- Requires thinking about resource usage upfront

The linear version is more explicit about how data flows through the algorithm, which can be both a feature (clarity, safety) and a drawback (verbosity, complexity).

### Linear Function Arrow `-o` (lollipop)

`List a -o List! b` - linear function arrow (consumes input)

Linear Function Arrow `-o` Examples

```elm
module LinearFunctions exposing (..)

-- REGULAR FUNCTION ARROW (->)
-- Can use the argument multiple times
duplicate : a -> (a, a)
duplicate x =
    (x, x)  -- x is used twice - this is fine!


-- LINEAR FUNCTION ARROW (-o)
-- Must use the argument exactly once
consume : a -o ()
consume x! =
    let
        _ = x!  -- x! is consumed here
    in
    ()


-- FILE HANDLE EXAMPLE
-- Linear types shine for resource management

type FileHandle!
    = FileHandle! String


-- Opening a file returns a linear file handle
openFile : String -> FileHandle!
openFile path =
    FileHandle! path


-- Reading consumes the handle and returns (content, new handle)
-- The signature ensures you can't use the old handle after reading
readFile : FileHandle! -o (String, FileHandle!)
readFile handle! =
    case! handle! of
        FileHandle! path ->
            let
                content = nativeRead path  -- hypothetical native function
            in
            (content, FileHandle! path)


-- Closing consumes the handle permanently
closeFile : FileHandle! -o ()
closeFile handle! =
    case! handle! of
        FileHandle! path ->
            nativeClose path  -- hypothetical native function
            ()


-- Example usage - compiler ensures proper cleanup
readAndClose : String -> String
readAndClose path =
    let
        handle1! = openFile path
        (content, handle2!) = readFile handle1!  -- handle1! consumed, can't use again
        _ = closeFile handle2!  -- handle2! consumed
    in
    content


-- INVALID EXAMPLE - Won't compile!
badExample : String -> String
badExample path =
    let
        handle! = openFile path
        (content1, handle2!) = readFile handle!
        (content2, handle3!) = readFile handle!  -- ERROR! handle! already consumed above!
        _ = closeFile handle2!
        _ = closeFile handle3!
    in
    content1 ++ content2


-- BUILDER PATTERN EXAMPLE

type StringBuilder!
    = StringBuilder! (List String)


empty : StringBuilder!
empty =
    StringBuilder! []


-- Linear arrow ensures builder is consumed and new one returned
append : String -> StringBuilder! -o StringBuilder!
append str builder! =
    case! builder! of
        StringBuilder! parts ->
            StringBuilder! (str :: parts)


build : StringBuilder! -o String
build builder! =
    case! builder! of
        StringBuilder! parts ->
            String.join "" (List.reverse parts)


-- Chaining works because each operation consumes and returns
makeGreeting : String -> String
makeGreeting name =
    empty
        |> append "Hello, "
        |> append name
        |> append "!"
        |> build


-- COMBINING LINEAR AND NON-LINEAR

-- Non-linear data, linear function
map : (a -> b) -> List! a -o List! b
map f list! =
    case! list! of
        [] ->
            []

        x :: xs! ->
            f x :: map f xs!


-- Linear data, linear function
mapLinear : (a -o b) -> List! a -o List! b
mapLinear f! list! =
    case! list! of
        [] ->
            []

        x :: xs! ->
            f! x :: mapLinear f! xs!


-- RESOURCE POOL EXAMPLE

type Pool! resource
    = Pool! (List resource)


-- Take from pool: consumes pool, returns (maybe resource, new pool)
take : Pool! a -o (Maybe a, Pool! a)
take pool! =
    case! pool! of
        Pool! resources ->
            case resources of
                [] ->
                    (Nothing, Pool! [])

                r :: rs ->
                    (Just r, Pool! rs)


-- Return to pool: consumes both resource and pool
putBack : a -> Pool! a -o Pool! a
putBack resource pool! =
    case! pool! of
        Pool! resources ->
            Pool! (resource :: resources)


-- Using the pool
usePool : Pool! String -o (String, Pool! String)
usePool pool! =
    let
        (maybeResource, pool2!) = take pool!  -- pool! consumed

        result =
            case maybeResource of
                Just resource ->
                    "Got: " ++ resource

                Nothing ->
                    "Pool empty"

        pool3! =
            case maybeResource of
                Just resource ->
                    putBack resource pool2!  -- pool2! consumed

                Nothing ->
                    pool2!  -- pool2! consumed
    in
    (result, pool3!)
```

Key Insights:

- `-o` enforces single use: The input must be used exactly once in the function body
- Type signature tells ownership story:
  - `a -> b` - "I'll look at `a` but you still own it"

  - `a -o b` - "Give me `a`, I'll consume it, you can't use it anymore"

- Chaining is natural: Since each function returns a new linear value, you can chain operations

- Prevents double-free: The type system ensures you can't close a file twice or free memory twice
  Explicit resource flow: You can trace exactly where resources go by following the linear arrows

The linear arrow makes resource management bugs into compile-time errors rather than runtime crashes!
