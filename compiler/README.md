# GDFunc Compiler

Core compiler library and CLI tool for the GDFunc programming language.

## Overview

GDFunc is an Elm-inspired functional programming language with linear types for the Godot VM. This package contains the compiler pipeline:

- **Scanner** - Lexical analysis and tokenization
- **Parser** - Parsing and AST construction following Elm's patterns
- **Type Checker** - Type inference with linear type checking
- **Code Generator** - Generates output for the Godot VM
- **Pretty Printer** - AST pretty printing

## Building

```bash
# From the compiler directory
cabal build

# Or from the root
cabal build GDFunc-compiler
```

## Testing

```bash
cabal test

# With details
cabal test --test-show-details=streaming
```

## Using the CLI

```bash
cabal run gdfunc -- [options] <file>
```

## Library Usage

```haskell
import GDFunc.Scanner (scanTokens)
import GDFunc.Parser (parseModule)
import GDFunc.TypeChecker (typeCheck)
import GDFunc.CodeGen (generateCode)

-- Compile a GDFunc file
compile :: String -> Either String String
compile source = do
    tokens <- scanTokens source
    ast <- parseModule tokens
    typedAst <- typeCheck ast
    code <- generateCode typedAst
    return code
```

## Module Structure

```
GDFunc/
├── Scanner.hs      - Lexer/tokenizer
├── Parser.hs       - Parser and AST definitions
├── TypeChecker.hs  - Type inference and checking
├── CodeGen.hs      - Code generation
└── Pretty.hs       - Pretty printer
```

## Examples

See the `../examples/` directory for GDFunc code examples:
- `factorial.gdfunc` - Factorial with recursion
- `borrowing_example.gdfunc` - Borrowed references
- `shared_types.gdfunc` - Shared types
- `linear_default.gdfunc` - Linear types by default
- `quicksort_linear.gdfunc` - Linear quicksort

## Development

### Running tests during development
```bash
cabal test --test-show-details=streaming
```

### Benchmarking
```bash
cabal bench
```

### Adding dependencies
Edit `GDFunc-compiler.cabal` and add to the `build-depends` section.
