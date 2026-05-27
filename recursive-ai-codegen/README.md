# recursive-ai-codegen

`recursive-ai-codegen` is a small Haskell controller for typed expression search and recursive code generation. It can score candidate ASTs with a deterministic fake scorer or with an LM Studio OpenAI-compatible local model, expand typed holes, wrap expressions into Haskell functions, run generated tests through `runghc`, and append training-ready JSONL logs.

## Architecture

- `AST` defines simple typed expressions such as variables, literals, arithmetic, lists, conditionals, and typed holes.
- `Generate` builds complete expression candidates for a goal type.
- `Score` scores expressions through fake, LM Studio, or fallback scoring.
- `LLM` sends JSON-schema scoring requests to LM Studio at the configured chat-completions endpoint.
- `Holes` finds typed holes, generates replacement candidates, scores each expansion, and chooses the best next partial expression.
- `Memory` provides reusable engram-style expression patterns such as `sum nums`, `length nums`, `x + y`, and list patterns.
- `FunctionGen` renders generated expressions as complete Haskell functions and test programs.
- `TestRunner` writes demo source files under `data/` and runs them with `runghc`.
- `TrainLog` appends JSONL rows for search, hole expansion, and function test feedback.

## Commands

```sh
cabal build
cabal run fake
cabal run llm
cabal run recursive-ai-codegen -- fake
```

Bare `cabal run` is ambiguous because this package exposes three executables: `fake`, `llm`, and `recursive-ai-codegen`.

## LM Studio

The default local endpoint is:

```text
http://127.0.0.1:1234/v1/chat/completions
```

Useful environment variables:

```sh
LLM_BASE_URL=http://127.0.0.1:1234/v1/chat/completions
LLM_MODEL=openai/gpt-oss-20b
LLM_MAX_TOKENS=256
LLM_PARALLEL_REQUESTS=8
LLM_DEBUG_RESPONSE=1
```

`llm` mode tries LM Studio first. If the request fails, scoring falls back to the Haskell fake scorer and records `fallback` as the score source.
`LLM_PARALLEL_REQUESTS` controls how many scoring requests the Haskell process sends to LM Studio at once.
Set `LLM_DEBUG_RESPONSE=1` only when you need to print the raw LM Studio JSON response for debugging.

## Generated Data

Runtime output is written under `data/`:

- `data/training-log.jsonl` contains append-only search, hole expansion, and function test rows.
- `data/GeneratedSumDemo.hs` is the generated and tested `generatedSum` program.
- `data/GeneratedLengthDemo.hs` is the generated and tested `generatedLength` program.

Recent JSONL rows include:

- `kind`: `search`, `hole_expansion`, or `function_test`
- `mode`: `fake` or `llm`
- goal type and environment
- expansion candidates, scores, score source, and candidate origin
- available memory pattern names
- chosen replacement or generated function body
- generated source path and source text
- compile/test pass or fail result with stdout and stderr

## v1 Includes

- deterministic fake scoring
- LM Studio JSON-schema scoring
- typed holes and recursive expansion
- reusable memory pattern candidates
- function source generation
- `runghc` compile/test feedback
- training-ready JSONL logs

## Not Included Yet

This v1 controller does not train a custom model. It prepares structured logs and feedback data that can be used for a later training pipeline.
