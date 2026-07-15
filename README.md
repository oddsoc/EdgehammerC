# asjcc

**asjcc** is an in-development C compiler based on the book *Writing a C Compiler* by Nora Sandler.

The compiler currently supports all C language features required to pass the book's official test suite up through **chapter 15**, including every extra-credit feature. I have also added support for some features not covered by the book, for example, asjcc can handle lazy array initialisers with nested brackets omitted (see tests/array_decls.c for examples that are handled). 

The long-term vision for asjcc is to serve as a playground for exploring language design. For now, I am focussed on working through the book with a secondary goal of learning Rust as I go along.

Each chapter corresponds to a single commit in the repository, the only exception is chapters 6 and 7 — the test cases for chapter 7 passed as a side effect of changes I made for chapter 6 (oops!). 

To keep the history aligned with the book, commits are limited to chapter completions. Each commit will contain the changes needed to pass the chapter's test cases and will also incorporate any refactoring, bug fixes, and general improvements made since completing the previous chapter.

While the implementation mostly follows the book’s approach, some areas differ, asjcc has a hand written tokeniser for example (for more on that see [performance](#tokeniser-performance)).

---

## Current Limitations

The only platform currently supported is Linux on x86_64.

---

## Building

asjcc targets stable Rust (edition **2024**) and depends only on the Rust standard library and unicode-ident crate. However, the preprocessor and tokeniser can take advantage of SIMD acceleration when built with the nightly Rust compiler - just enable the "simd" feature. 

To build without SIMD acceleration:

```bash
cargo build -r
```

or, with SIMD acceleration:

```bash
cargo +nightly build -r --features "simd"
```

## Tokeniser Performance

I've measured the performance of the tokeniser (using Criterion) at **~945MiB/s (4.55 cpb)** on the sqlite3.c amalgamation* with SIMD enabled, and **~644MiB/s (6.68 cpb)** without. Tested on an AMD EPYC 9655, YMMV.

*preprocessed to remove # directives but with comments left in place.

To run the benchmark yourself, with simd:

```bash
cargo +nightly bench --features "simd" --bench tokeniser
```

or, without simd:

```bash
cargo bench --bench tokeniser
```

---

## Usage

```
asjcc [options] <file.c> [<file.c> ...]
```

asjcc compiles one or more C source files into an executable. It calls out to `cc` (gcc or clang) to assemble and link, so a C toolchain must be installed on your system.

### Examples

Compile a single file — produces an executable named `prog` in the current directory:

```bash
asjcc prog.c
```

Compile and write the output to a specific path:

```bash
asjcc -o build/my_program src/main.c
```

Compile to an object file without linking:

```bash
asjcc -c prog.c
```

Produce assembly output (`prog.s` written to the current directory):

```bash
asjcc -S prog.c
```

Compile multiple source files into a single executable:

```bash
asjcc -o my_app src/a.c src/b.c src/c.c
```

Link against an external library:

```bash
asjcc -lm -o math_app main.c
```

### Options

| Flag | Description |
|---|---|
| `-o <path>` | Write output to `<path>`. Defaults to `<name>` for single-file builds or `a.out` for multi-file builds. |
| `-c` | Compile only — produce `.o` object files without linking. |
| `-S` | Produce assembly (`.s`) files in the current directory instead of a final executable. |
| `-l <lib>` | Link against library `<lib>`. Can be specified multiple times. Passed through to `cc` as `-l<lib>`. |
| `--lex` | Stop after tokenisation — print the token stream and exit. |
| `--parse` | Stop after parsing — run the tokeniser and parser, then exit. |
| `--validate` | Run the parser and full semantic analysis (type checking, etc.) without generating code. |
| `--codegen` | Run the full pipeline and print intermediate representations (three-address-code, MIR) to stdout. |
| `--help` | Print usage information and exit. |

### Exit behaviour

- If no source files are provided, the compiler exits with an error.
- `-o` can only be specified once; specifying it twice is an error.
- Unknown options cause the compiler to exit with an error message.
