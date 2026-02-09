# GDFunc Migration Guide: Version 0.1 to 1.0

This guide explains the changes made to GDFunc to implement the new linear-by-default specification.

## Summary of Changes

The codebase has been updated to implement the new LANGSPECS.md specification, which introduces a **linear-by-default** type system with explicit borrowing and shared types.

### Key Differences

| Feature | Old Syntax | New Syntax |
|---------|-----------|------------|
| Linear values | `list!` | `list` (default) |
| Linear types | `List! Int` | `List Int` (default) |
| Borrowed values | N/A | `&list` |
| Borrowed types | N/A | `&List Int` |
| Shared types | N/A | `type shared Point = ...` |
| Basic types | Unrestricted | Shared by default (Int, Float, String, Bool, Char) |

## Detailed Changes

### 1. Scanner (src/GDFunc/Scanner.hs)

**Added:**
- `AMPERSAND` token for the `&` borrowing operator
- `SHARED` keyword for shared type declarations

**Changed:**
- Single `&` now creates `AMPERSAND` token instead of error
- Added `shared` to keywords list

### 2. Parser (src/GDFunc/Parser.hs)

**Type System Changes:**

```haskell
-- Old:
data Type
    = TLinear Type  -- Type!
    | ...

-- New:
data Type
    = TBorrowed Type   -- &Type (borrowed/non-consuming)
    | TShared Type     -- shared Type (can be freely copied)
    | ...
```

**Expression Changes:**

```haskell
-- Old:
data Expr
    = ELinearVar String  -- variable!
    | ...

-- New:
data Expr
    = EBorrow Expr       -- &expr (borrow expression)
    | ...
```

**Pattern Changes:**

```haskell
-- Old:
data Pattern
    = PLinearVar String  -- x!
    | ...

-- New:
data Pattern
    = PBorrow Pattern    -- &pattern (borrowed pattern)
    | ...
```

**Declaration Changes:**

```haskell
-- New:
data Declaration
    = SharedTypeDecl String [String] [(String, [Type])]  -- type shared Name = ...
    | SharedTypeAlias String [String] Type               -- type shared alias Name = ...
    | ...
```

### 3. TypeChecker (src/GDFunc/TypeChecker.hs)

**Semantic Changes:**

1. **Linear-by-default**: All user-defined types are now linear by default (must be used exactly once)
2. **Basic types are shared**: Int, Float, String, Bool, Char are automatically shared
3. **Borrowing semantics**: Borrowed values can be used multiple times without consuming them
4. **Shared type semantics**: Types marked `shared` can be freely copied

**New Functions:**
- `markBorrowed :: String -> LinTypeCheck ()`
- `markSharedType :: String -> LinTypeCheck ()`
- `isTypeShared :: P.Type -> LinTypeCheck Bool`
- `isVarBorrowed :: String -> LinTypeCheck Bool`

**Updated Type Environment:**
- Added `ctxBorrowedVars :: Set String` to track borrowed variables
- Added `ctxSharedTypes :: Set String` to track shared type names
- Basic types (Int, Float, String, Bool, Char) are pre-registered as shared

### 4. Pretty Printer (src/GDFunc/Pretty.hs)

Updated to display the new syntax:
- `TBorrowed typ` → `"&" ++ prettyTypeAtom typ`
- `TShared typ` → `"shared " ++ prettyTypeAtom typ`
- `EBorrow expr` → `"&" ++ prettyExprAtom expr`
- `PBorrow pattern` → `"&" ++ prettyPatternAtom pattern`

## Migration Examples

### Example 1: Linear Variables

**Old:**
```elm
-- Mark variable as linear with !
process : List! Int -> Int
process list! =
    case! list! of
        [] -> 0
        x :: xs! -> x + process xs!
```

**New:**
```elm
-- Variables are linear by default (no ! needed)
process : List Int -> Int
process list =
    case list of
        [] -> 0
        x :: xs -> x + process xs
```

### Example 2: Borrowing (Non-consuming Access)

**Old:**
```elm
-- No borrowing - had to consume the list
length : List a -> Int
```

**New:**
```elm
-- Use & to borrow (non-consuming access)
length : &List a -> Int
length list =
    case list of
        [] -> 0
        _ :: rest -> 1 + length &rest

-- Usage:
main =
    let numbers = [1, 2, 3]
        len = length &numbers  -- borrow
        total = sum numbers     -- consume
    in len + total
```

### Example 3: Shared Types

**New feature:**
```elm
-- Shared types can be used multiple times
type shared Point = Point Float Float
type shared Config = Config
    { host : String
    , port : Int
    }

-- Function can use config multiple times (no borrowing needed)
processWithConfig : Config -> String -> String
processWithConfig config data =
    let url = makeUrl config           -- use 1
        result = sendRequest config url data  -- use 2
    in result
```

### Example 4: Basic Types Are Shared

**New:**
```elm
-- Int, Float, String, Bool, Char are automatically shared
duplicate : Int -> (Int, Int)
duplicate x = (x, x)  -- OK - Int is shared

-- Old behavior would have required:
-- duplicate : Int -> (Int, Int)
-- duplicate x = error "Can't use x twice"
```

## Updated Examples

Three new example files demonstrate the new features:

1. **[borrowing_example.gdfunc](examples/borrowing_example.gdfunc)** - Shows how to use `&` for non-consuming access
2. **[shared_types.gdfunc](examples/shared_types.gdfunc)** - Demonstrates shared type declarations
3. **[linear_default.gdfunc](examples/linear_default.gdfunc)** - Shows linear-by-default behavior

The existing examples have been updated:
- **[factorial.gdfunc](examples/factorial.gdfunc)** - Still works (uses shared Int types)
- **[quicksort_linear.gdfunc](examples/quicksort_linear.gdfunc)** - Updated to new syntax

## Type System Rules

### Linearity Rules

1. **Linear values** (default for user-defined types):
   - Must be used exactly once
   - Cannot be duplicated or discarded
   - Example: `List a`, custom data types

2. **Borrowed values** (`&value`):
   - Can be used multiple times
   - Read-only access
   - Cannot be converted to owned
   - Example: `&list`, `&config`

3. **Shared values** (types marked `shared`):
   - Can be freely copied and used multiple times
   - No linearity restrictions
   - Example: `Int`, `Float`, `String`, `Bool`, `Char`, `type shared Point = ...`

### Type Signatures

```elm
-- Consuming function (default)
consume : List a -> Int

-- Borrowing function
inspect : &List a -> Int

-- Shared type
type shared Config = Config { ... }

-- Mixed
process : &Config -> List a -> List a
```

## Testing

Updated test files:
- **[test/GDFunc/ParserSpec.hs](test/GDFunc/ParserSpec.hs)** - Tests for `&` and `shared` parsing
- **[test/GDFunc/ScannerSpec.hs](test/GDFunc/ScannerSpec.hs)** - Tests for `AMPERSAND` and `SHARED` tokens

Run tests with:
```bash
cabal test
```

## Breaking Changes

⚠️ **Warning**: This is a breaking change from the previous version.

Old code using `!` syntax will **not work** and must be migrated:

1. Remove all `!` suffixes from variable names and type names
2. Add `&` prefix where borrowing is needed
3. Mark types as `shared` where appropriate
4. Basic types (Int, Float, String, Bool, Char) are now automatically shared

## Benefits of the New System

1. **Clearer intent**: Linear is the default, making ownership explicit
2. **Better ergonomics**: No need to mark every linear variable with `!`
3. **Borrowing**: Explicit non-consuming access with `&`
4. **Shared types**: Clear distinction between copyable and linear types
5. **Resource safety**: Still maintains compile-time resource management

## Additional Resources

- [LANGSPECS.md](LANGSPECS.md) - Complete language specification
- [examples/](examples/) - Updated example programs
- [Rust ownership](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html) - Similar ownership model
- [Linear Haskell](https://arxiv.org/abs/1710.09756) - Academic background

---

**Last Updated**: 2026-02-09
**Version**: 1.0
