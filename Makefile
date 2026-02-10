.PHONY: help build build-compiler build-web test test-compiler clean install run-cli run-web

help:
	@echo "GDFunc Monorepo - Available targets:"
	@echo ""
	@echo "  build            - Build all packages"
	@echo "  build-compiler   - Build only the compiler package"
	@echo "  build-web        - Build only the web server package"
	@echo "  test             - Run all tests"
	@echo "  test-compiler    - Run compiler tests"
	@echo "  clean            - Clean build artifacts"
	@echo "  install          - Install executables"
	@echo "  run-cli          - Run the CLI tool"
	@echo "  run-web          - Run the web server"
	@echo ""

build:
	cabal build all

build-compiler:
	cabal build GDFunc-compiler

build-web:
	@echo "Note: Uncomment 'web/' in cabal.project first"
	cabal build GDFunc-web

test:
	cabal test all

test-compiler:
	cabal test GDFunc-compiler --test-show-details=streaming

clean:
	cabal clean
	rm -rf dist-newstyle

install:
	cabal install all

run-cli:
	cabal run gdfunc

run-web:
	cabal run gdfunc-web

# Development targets
.PHONY: watch-test watch-build dev

watch-test:
	@echo "Watching for changes and running tests..."
	@while true; do \
		inotifywait -e modify -r compiler/src compiler/test 2>/dev/null || \
		fswatch -1 -r compiler/src compiler/test; \
		clear; \
		make test-compiler; \
	done

dev:
	@echo "Development mode - building and watching for changes"
	make build-compiler
	make watch-test
