# GDFunc

An experimental functional programming language with linear types, inspired by Elm, designed for the Godot VM.

## Features

- **Elm-like Syntax**: Clean, functional syntax inspired by Elm
- **Linear Types by Default**: Resources are linear by default, preventing accidental duplication
- **Borrowing System**: Explicit `&` syntax for borrowed (non-consuming) references
- **Shared Types**: Opt-in `shared` keyword for copyable types
- **Type Inference**: Full Hindley-Milner type inference with linear type checking
- **Pattern Matching**: Comprehensive pattern matching with linear consumption tracking

## Monorepo Structure

This project is organized as a monorepo with two main packages:

```
gdfunc/
├── compiler/           # Core compiler (GDFunc-compiler)
│   ├── src/           # Scanner, Parser, TypeChecker, CodeGen
│   ├── app/           # CLI executable
│   ├── test/          # Test suite
│   └── bench/         # Benchmarks
└── web/               # Web server (GDFunc-web, optional)
    ├── src/           # Web API
    └── app/           # Web server executable
```

See [MONOREPO.md](MONOREPO.md) for detailed structure documentation.

## Quick Start

### Building

```bash
# Build the compiler
make build-compiler

# Or use cabal directly
cabal build GDFunc-compiler
```

### Installation

```bash
cabal install GDFunc-compiler
```

### Running Tests

```bash
make test-compiler
# Or: cabal test GDFunc-compiler
```

Current test status: **51 passing, 3 failing** (parser improvements in progress)

### Example Program

Create a file `factorial.gdfunc`:

```elm
module Factorial exposing (main)

factorial : Int -> Int
factorial n =
    if n <= 1 then
        1
    else
        n * factorial (n - 1)

main : Int
main = factorial 5
```

Compile and run:

```bash
# After installation
gdfunc factorial.gdfunc

# Or during development
cabal run gdfunc -- factorial.gdfunc
```

## Language Examples

### Borrowing (Non-Consuming References)

```elm
-- Length function borrows the list without consuming it
length : &List a -> Int
length list =
    case list of
        [] -> 0
        _ :: rest -> 1 + length &rest

-- The list can be used multiple times
main =
    let myList = [1, 2, 3]
        len = length &myList
        sum = sumList myList  -- myList still available
    in len + sum
```

### Shared Types (Copyable)

```elm
-- Shared types can be freely copied
type shared Config = Config
    { host : String
    , port : Int
    }

-- No borrowing needed for shared types
useConfig : Config -> String
useConfig config =
    config.host ++ ":" ++ String.fromInt config.port
```

### Linear Types (Default)

```elm
-- Regular types are linear by default
type Stack a = Stack (List a)

-- Must consume the stack exactly once
push : a -> Stack a -> Stack a
push item (Stack items) =
    Stack (item :: items)
```

## Development

### Available Make Targets

```bash
make help              # Show all available targets
make build-compiler    # Build the compiler
make test-compiler     # Run tests with details
make clean             # Clean build artifacts
make run-cli           # Run the CLI tool
```

### Project Structure

- `compiler/` - Core compiler package
  - [README](compiler/README.md) - Compiler documentation
  - `src/GDFunc/` - Compiler modules
    - `Scanner.hs` - Lexical analysis
    - `Parser.hs` - Parsing (following Elm's approach)
    - `TypeChecker.hs` - Type inference with linear types
    - `CodeGen.hs` - Code generation
    - `Pretty.hs` - Pretty printing

- `web/` - Web server package (optional, requires dependency fixes)
  - [README](web/README.md) - Web server documentation
  - `src/GDFunc/WebServer.hs` - REST API

- `examples/` - Example GDFunc programs
  - `factorial.gdfunc`
  - `borrowing_example.gdfunc`
  - `shared_types.gdfunc`
  - `linear_default.gdfunc`
  - `quicksort_linear.gdfunc`

### Recent Improvements

- ✅ Separated type annotations from function declarations (following Elm)
- ✅ Fixed greedy type parsing that was consuming tokens from next declarations
- ✅ Improved pattern parsing with proper backtracking
- ✅ Layout-sensitive parsing using NEWLINE tokens
- ✅ Refactored into monorepo structure

### Known Issues

- Multi-line expressions need better handling
- Type alias parsing with `(..)` in exposing clause
- Record types in type declarations need LEFT_BRACE support

## Documentation

- [MONOREPO.md](MONOREPO.md) - Monorepo structure and rationale
- [compiler/README.md](compiler/README.md) - Compiler documentation
- [web/README.md](web/README.md) - Web server documentation
- [CHANGELOG.md](CHANGELOG.md) - Version history

## Contributing

This is an experimental project exploring linear types in functional languages. Contributions and feedback are welcome!

### Development Workflow

1. Make changes to `compiler/src/`
2. Run tests: `make test-compiler`
3. Build: `make build-compiler`
4. Test CLI: `cabal run gdfunc -- examples/factorial.gdfunc`

## License

BSD-3-Clause (see LICENSE file)

## Acknowledgments

- Inspired by [Elm](https://elm-lang.org/) for syntax and philosophy
- Linear types concepts from Haskell's LinearTypes extension
- Parser implementation follows patterns from the [Elm compiler](https://github.com/elm/compiler)
