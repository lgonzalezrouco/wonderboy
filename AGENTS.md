# AGENTS.md — Wonder Boy (Haskell)

Guidelines for coding agents working in this repository.

## Build / Lint / Test

Project-wide Cabal options live in `cabal.project` (notably `test-show-details: direct`
and `write-ghc-environment-files: never`). For **haskell-language-server**, component
paths are mapped in `hie.yaml`.

```bash
cabal build all --enable-tests     # build library + executable + tests
cabal run wonderboy-hs             # run the game
cabal test all                     # run full test suite (details: direct via cabal.project)
cabal test all --test-show-details=direct   # same; explicit flag optional
cabal check                        # validate .cabal package description
cabal haddock all --enable-documentation    # build docs
```

### Linting & Formatting

```bash
hlint src app test                 # lint (uses default rules, no .hlint.yaml)
fourmolu --mode check src app test # check formatting (dry-run)
fourmolu --mode inplace src app test # auto-format
```

Fourmolu config is in `fourmolu.yaml` (2-space indent, leading commas,
trailing function arrows, diff-friendly imports). CI enforces both HLint
and Fourmolu — run them before committing.

### Running a Single Test

The test suite uses `exitcode-stdio-1.0` with no framework yet. To get
single-test granularity, add **Tasty** (or HSpec) to the test build-depends
and use `tasty-discover`. Then:

```bash
cabal test --test-option="--pattern=moduleName/testName"
```

Until a framework is added, all tests run together via `cabal test`.

## Architecture — Layered Purity

This project follows Clean Architecture / Hexagonal Architecture in Haskell.
Respect the layer boundaries strictly:

| Layer         | Purity                                                                    | What goes here                                                                                                          |
| ------------- | ------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| `Domain/`     | **100% pure** — no `IO`, no monad transformers touching the outside world | Value objects (`Position`, `Velocity`), domain models (`Player`, `Enemy`, `World`), pure logic (`Physics`, `Collision`) |
| `UseCases/`   | **Abstract monads** — typeclasses or newtypes, no concrete `IO`           | `GameMonad`, `UpdateGame`, port interfaces (`InputPort`, `RenderPort`, `TimePort`)                                      |
| `Adapters/`   | **Concrete implementations** of ports                                     | `GlossInput`, `GlossRenderer`, `SystemClock` — these may use `IO`                                                       |
| `Frameworks/` | **External wiring**                                                       | `Gloss.GameLoop` — ties adapters to the game loop                                                                       |

**Rules:**

- `Domain` modules must NEVER import from `UseCases`, `Adapters`, or `Frameworks`.
- `UseCases` may import `Domain` but NOT `Adapters` or `Frameworks`.
- `Adapters` may import `UseCases` (ports) and `Domain` (types).
- Dependency direction: `Frameworks → Adapters → UseCases → Domain`.

## Monads & Effects

- Use `StateT` for mutable game state, `ReaderT` for read-only config/environment,
  `ExceptT` (or `MonadError`) for recoverable errors.
- Define **Free monads** (or `Operational`-style) for the entity DSL — separate the
  _description_ of behaviour from its _interpretation_.
- Keep the pure core (`Domain`) free of any monad stack. Effects live in `UseCases`
  and above.
- Prefer `mtl`-style typeclass constraints (`MonadState`, `MonadReader`, `MonadError`)
  over hard-coding a concrete transformer stack.

## Code Style

### Formatting

- Enforced by **Fourmolu** — do not hand-format to override it.
- 2-space indentation, no tabs.
- Trailing commas in import/export lists (diff-friendly).
- Trailing `::` / function arrows.

### Imports

- Use **qualified imports** for modules with name clashes: `import qualified Data.Map.Strict as Map`.
- Prefer explicit import lists (`import Data.Maybe (fromMaybe)`) over open imports.
- Group imports: (1)stdlib, (2)third-party, (3)project modules, separated by blank lines.

### Naming

- Modules: `Domain.Model.Player`, `UseCases.Ports.InputPort` — mirror directory structure.
- Types / data constructors: `PascalCase` (`PlayerState`, `AABB`).
- Functions / values: `camelCase` (`updatePosition`, `gravity`).
- Typeclass methods: `camelCase` (`handleInput`, `renderFrame`).
- Use descriptive names; avoid single-letter identifiers except in short lambdas.

### Types

- Prefer `newtype` over `data` for single-field wrappers (e.g. `newtype Position = Position (Float, Float)`).
- Use smart constructors and export only the type, not raw data constructors,
  when invariants must be enforced.
- Derive `Eq`, `Show`, `Generic` where sensible. Add `NFData` for testable types.

### Error Handling

- Use `Either` / `ExceptT` for domain errors — never partial functions (`fromJust`, `!!`).
- `error` / `undefined` only in prototypes; replace before merging.
- Newtypes + smart constructors for validated values.

### Documentation

- Haddock comments (`-- |`) on all exported types and functions.
- `-- ^` for field documentation on record types.
- CI builds Haddock — broken docs will fail the build.

## GHC Warnings

All targets use `default-language: GHC2021` and share a `warnings` stanza in
`wonderboy-hs.cabal`: `-Wall`, `-Wcompat`, `-Widentities`,
`-Wmissing-deriving-strategies`, and `-Wpartial-fields`. Fix all warnings before committing.
Do not suppress warnings globally; use `{-# OPTIONS_GHC -Wno-... #-}` only
per-module with a comment explaining why.

## General Rules

- Run `cabal build all --enable-tests && fourmolu --mode check src app test && hlint src app test` before committing.
- Do not commit `dist-newstyle/`, `.hi`, `.o`, or other build artifacts.
- Keep PRs small and focused on one layer at a time.
