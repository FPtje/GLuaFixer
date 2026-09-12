# GLuaFixer

Haskell project: linter and pretty printer for Garry's Mod Lua. Built with Cabal and Nix.

## Build

```bash
# Using direnv (prefix with `nix develop --command` if direnv is not allowed)
./AGGenerator.sh    # generates .hs from .ag

cabal build glualint          # executable (the binary)
cabal build glualint:lib      # library (lexer, parser, AST, pretty-printer, lint rules)
cabal build glualint:linttest # unit test suite (Tasty + HUnit)
cabal build glualint:golden   # golden pretty-print test suite
cabal build all -j --enable-tests  # or build everything at once
```

`AGGenerator.sh` invokes `uuagc` (Uniqueness and Uniformity Attribute Grammar compiler) on 5 `.ag` sources. The generated `.hs` files are git-ignored and live in `src/GLua/AG/` and `src/GLuaFixer/AG/`. **Never edit the generated `.hs` — edit the `.ag` sources instead.**

## Test

```bash
cabal run linttest # unit tests
cabal run golden # golden pretty-print tests
```

Tests are in `tests/linttest/` (unit tests) and `tests/golden/` (golden output tests against `tests/golden/data/input/` and `tests/golden/data/output/`).

## Lint / style

```bash
cabal-fmt --check --Werror glualint.cabal # cabal format
fourmolu --mode inplace app src tests # fourmolu (with autofix)
hlint app src tests # hlint (see .hlint.yaml — Eta reduce ignored)
```

Style: two-space indentation, fourmolu with the config in `fourmolu.yaml`.

## Architecture

- **Source**: `src/GLua/` (lexer, parser, AST, pretty-printer), `src/GLuaFixer/` (lint rules, settings)
- **Executable**: `app/GLuaFixer/Main.hs` — uses Effectful effect system with layered effects (CLI, files, logging, interruptible, run)
- **Key module**: `src/GLuaFixer/Interface.hs` — `sourceLint` is the main entry point, orchestrates lex → parse → lexiconLint → astLint → prettyprint

## Code generation (AG files)

Attribute grammar sources (`.ag`) are compiled to Haskell by `uuagc`. Each `.ag` file maps to a generated `.hs`:

| .ag source | generated .hs |
| --- | --- |
| `src/GLua/AG/AST.ag` | `src/GLua/AG/AST.hs` |
| `src/GLua/AG/Token.ag` | `src/GLua/AG/Token.hs` |
| `src/GLua/AG/PrettyPrint.ag` | `src/GLua/AG/PrettyPrint.hs` |
| `src/GLuaFixer/AG/LexLint.ag` | `src/GLuaFixer/AG/LexLint.hs` |
| `src/GLuaFixer/AG/ASTLint.ag` | `src/GLuaFixer/AG/ASTLint.hs` |

## Release

1. Check if `CACHIX_AUTH_TOKEN` is in the environment. If not, stop immediately and ask to re-open harness with environment variable set.
2. If not provided, ask what the version number should be. Mention the current version number from `glualint.cabal`
3. Update the version in `glualint.cabal`, run `git add glualint.cabal && git commit -m "<version number here>"`. Do not push.
4. Create a git tag with the name being just the version number. Do not push the tag
5. Run the release scripts `release-linux.sh`, or `release-aarch64-linux.sh` (requires `CACHIX_AUTH_TOKEN`). Run the script that matches the current architecture.
6. Generate a human readable changelog, based on the git commits since last release. Give it as answer, and ask for human to push commit and tags, and to then upload output files to GitHub's tag release at `https://github.com/FPtje/GLuaFixer/releases/tag/<version number>`. Ask the user to also run the release script for the other architecture and upload that.

## Gotchas

- **Must run AGGenerator.sh before cabal build.** The lint workflow checks fourmolu before codegen — `.ag`-generated files are exempt from fourmolu.
- `testcase.lua` is git-ignored (used by `cabal run glualint -- test`).
- Avoid running commands through `nix develop --command`, unless command-not-found errors imply direnv is not available or enabled.
