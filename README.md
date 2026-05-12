# GDFunc

An experimental functional language with linear types, inspired by Elm, that compiles to **GDScript** (for the [Godot](https://godotengine.org/) game engine) and **C**.

> **Status: pre-alpha / experimental.** The frontend (scanner, parser, type checker) is partially working. The C backend in [CodeGen.hs](compiler/src/GDFunc/CodeGen.hs) is functional but limited; the GDScript backend is in progress. Expect breakage.

## How it works

```
                                            ┌─►  .gd  (GDScript, for Godot)
.gdfunc  →  Scanner  →  Parser  →  TypeChecker  ─┤
                                            └─►  .c   (C, native)
```

You write Elm-flavored source with linear types and borrowing. The compiler type-checks ownership, lowers those concepts away (neither GDScript nor C has an ownership system), and emits code for the selected target — GDScript files you can drop into a Godot project, or C you can hand to any C toolchain.

## Features

- **Elm-like syntax** — clean, functional, indentation-sensitive
- **Linear types by default** — values must be used exactly once
- **Borrowing** — explicit `&` for non-consuming references
- **Primitives are implicitly copyable** — `Int`, `Float`, `String`, `Bool`, `Char` need no special syntax
- **Hindley–Milner type inference** with linear checking
- **Pattern matching** with linear-consumption tracking

See [LANGSPECS.md](LANGSPECS.md) for the full language specification.

## A tiny example

`factorial.gdfunc`:

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
cabal run gdfunc -- compiler/examples/factorial.gdfunc
```

More examples live in [compiler/examples/](compiler/examples/):
`factorial.gdfunc`, `borrowing_example.gdfunc`, `shared_types.gdfunc`,
`linear_default.gdfunc`, `quicksort_linear.gdfunc`.

## Getting started

### Prerequisites

One of:

- **Nix** (recommended) — handles GHC and cabal for you. Install from [nixos.org/download](https://nixos.org/download/), then `./dev-shell.sh`.
- **Manual toolchain** — GHC ≥ 9.4 and Cabal ≥ 3.10, e.g. via [ghcup](https://www.haskell.org/ghcup/).

### Build, test, install

```bash
make build-compiler        # cabal build GDFunc-compiler
make test-compiler         # cabal test GDFunc-compiler
cabal install GDFunc-compiler   # installs `gdfunc` to ~/.cabal/bin (add to PATH)
```

Run `make help` for the full target list.

After installation:

```bash
# Compile to C (default target) and link with cc
gdfunc path/to/program.gdfunc

# Emit GDScript for Godot
gdfunc --target gdscript path/to/program.gdfunc

# Emit code without invoking cc
gdfunc --emit-only path/to/program.gdfunc

# Type-check only
gdfunc --check path/to/program.gdfunc

# Run `gdfunc --help` for the full flag list.
```

## Language examples

### Borrowing — non-consuming references

```elm
-- `length` borrows the list rather than consuming it
length : &List a -> Int
length list =
    case list of
        [] -> 0
        _ :: rest -> 1 + length &rest   -- re-borrow the tail

main =
    let myList = [1, 2, 3]
        len = length &myList
        sum = sumList myList            -- myList still available afterwards
    in len + sum
```

### Linear types — the default

```elm
type Stack a = Stack (List a)

-- Must consume the stack exactly once
push : a -> Stack a -> Stack a
push item (Stack items) =
    Stack (item :: items)
```

## Project layout

```
gdfunc/
├── compiler/                         # core compiler — GDFunc-compiler
│   ├── src/GDFunc/
│   │   ├── Scanner.hs                # lexical analysis
│   │   ├── Parser.hs                 # parser (Elm-style)
│   │   ├── TypeChecker.hs            # HM inference + linear checking
│   │   ├── CodeGen.hs                # C + GDScript emission (GDScript in progress)
│   │   └── Pretty.hs                 # pretty printing
│   ├── app/                          # `gdfunc` CLI
│   ├── test/                         # test suite
│   ├── bench/                        # benchmarks
│   └── examples/                     # .gdfunc sample programs
├── web/                              # experimental web server (see web/README)
├── LANGSPECS.md                      # language specification
└── CHANGELOG.md
```

## Contributing

This is an experimental project exploring linear types in a small functional language with a practical compile target. Feedback, bug reports, and PRs are welcome.

Typical workflow:

1. Edit files under [compiler/src/](compiler/src/)
2. `make test-compiler`
3. `cabal run gdfunc -- compiler/examples/factorial.gdfunc`

<details>
<summary>Web server package (experimental)</summary>

[web/](web/) contains an optional HTTP API around the compiler. It is not currently wired into `cabal.project` and may need dependency fixes before it builds. See [web/README.md](web/README.md).
</details>

## License

[BSD-3-Clause](LICENSE).

## Acknowledgments

- Syntax and philosophy inspired by [Elm](https://elm-lang.org/)
- Linear-type concepts from Haskell's `LinearTypes` extension
- Parser patterns adapted from the [Elm compiler](https://github.com/elm/compiler)
- Compile targets: [GDScript](https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/gdscript_basics.html) / [Godot](https://godotengine.org/) and C
