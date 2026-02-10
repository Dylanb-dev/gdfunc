# GDFunc Web Server

Web interface for the GDFunc compiler, providing a REST API for browser-based compilation.

## Overview

This package provides a Scotty-based web server that exposes the GDFunc compiler functionality via HTTP endpoints. It allows:

- Compiling GDFunc code through a REST API
- Running compiled code
- Interactive web-based development

## Building

**Note**: The web server has dependency conflicts with newer GHC versions. To build it, uncomment the web package in `../cabal.project`:

```cabal
packages:
  compiler/
  web/  -- Uncomment this line
```

Then:

```bash
cabal build GDFunc-web
```

## Running

```bash
cabal run gdfunc-web
```

The server will start on port 3000 by default.

## API Endpoints

### POST /compile
Compiles GDFunc source code.

**Request:**
```json
{
  "source": "factorial : Int -> Int\nfactorial n = if n <= 1 then 1 else n * factorial (n - 1)"
}
```

**Response:**
```json
{
  "success": true,
  "output": "compiled code...",
  "errors": []
}
```

### POST /run
Compiles and runs GDFunc code.

**Request:**
```json
{
  "source": "main = 42"
}
```

**Response:**
```json
{
  "success": true,
  "output": "42",
  "errors": []
}
```

## Configuration

The server can be configured via `ServerConfig`:

```haskell
import GDFunc.WebServer

main = runWebServer $ ServerConfig
  { port = 3000
  , staticDir = "./static"
  }
```

## Dependencies

This package depends on:
- `GDFunc-compiler` - The core compiler
- `scotty` - Web framework
- `aeson` - JSON serialization
- `wai-*` - Web application interface middleware

## Development

### Working on the web server

```bash
cd web
cabal build
cabal run gdfunc-web
```

### Testing the API

```bash
# Test compilation
curl -X POST http://localhost:3000/compile \
  -H "Content-Type: application/json" \
  -d '{"source": "main = 42"}'

# Test running code
curl -X POST http://localhost:3000/run \
  -H "Content-Type: application/json" \
  -d '{"source": "main = 42"}'
```

## Troubleshooting

### Dependency Conflicts

If you encounter dependency conflicts (especially with `aeson` and `base`), you can:

1. Use allow-newer in `cabal.project`:
```cabal
package GDFunc-web
  allow-newer:
    aeson:*,
    indexed-traversable-instances:base
```

2. Use an older GHC version (9.8 or earlier)

3. Wait for dependency updates to support newer base versions
