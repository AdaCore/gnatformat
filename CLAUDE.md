# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

GNATformat is an opinionated Ada source code formatter built on
[Prettier-Ada](https://github.com/AdaCore/prettier-ada) (a port of Prettier). It formats Ada code
following the [GNAT Coding Style](https://gcc.gnu.org/onlinedocs/gnat-style.pdf) guide. It ships as
both a command-line tool and a library (used by the Ada Language Server).

## Build Commands

```sh
# Build both the library (all three variants) and the binary
make all

# Build only the library (static, static-pic, relocatable)
make lib

# Build only the command-line binary
make bin

# Install (set PREFIX to desired location)
PREFIX=/usr/local make install

# Clean build artifacts (bin/, lib/, obj/)
make clean
```

Build mode defaults to `dev`. Use `BUILD_MODE=prod` for optimized production builds.

GPR project files are in `gnat/`:
- `gnatformat_common.gpr` — shared compiler switches (abstract project)
- `gnatformat.gpr` — the library project (sources from `src/`)
- `gnatformat_driver.gpr` — the binary project (sources from `src/formatters/ada/`)
- `git_gnatformat.gpr` — the standalone `git-gnatformat` subcommand wrapper (sources from `src/formatters/git/`); depends only on the Ada runtime, not the library

## Running Tests

```sh
# Run the full testsuite
make test

# Run a specific test by directory path
python testsuite/testsuite.py testsuite/tests/path/to/test

# Rewrite test baselines to match current output
python testsuite/testsuite.py -r

# Run with Valgrind
python testsuite/testsuite.py --valgrind
```

Tests use the [e3-testsuite](https://e3-testsuite.readthedocs.io/en/latest/) framework. Each test
directory contains a `test.yaml` (driver config and args) and a `test.out` (expected output
baseline). Install test deps with `pip install -r requirements-dev.txt`.

## Code Architecture

### Library (`src/`)

The public library API lives in `src/`:

- `gnatformat.ads` — root package; defines `Supported_Languages`, version constants, and the `Gnatformat_Trace` handle
- `gnatformat-configuration.ads/.adb` — `Format_Options_Type` (width, indentation, charset, end-of-line, keyword casing, ignore); `Format_Options_Builder_Type` for constructing options; `From_Project` to read options from a GPR2 project's `package Format`; `Load_Unparsing_Configuration` for formatting rules
- `gnatformat-formatting.ads/.adb` — core `Format` and `Range_Format` functions; takes an `Analysis_Unit` + `Format_Options_Type` and returns formatted text or a `Formatting_Edit_Type`
- `gnatformat-edits.ads/.adb` — `Text_Edit_Type` / `Formatting_Edit_Type` / `Formatting_Edits_Type`; `Apply_Edits` to write edits to disk
- `gnatformat-helpers.ads/.adb` — internal formatting helpers
- `gnatformat-utils.ads` — generic `Optional` type used throughout

### Driver (`src/formatters/ada/`)

The CLI binary is assembled here:

- `gnatformat-ada_driver.adb` — main entry point; parses CLI args, loads project, dispatches to `Full_Format` or `Range_Format`
- `gnatformat-command_line.ads` — CLI argument declarations
- `gnatformat-command_line-configuration.ads/.adb` — maps CLI flags to `Format_Options_Type`
- `gnatformat-project.ads/.adb` — GPR2 project loading and source discovery
- `gnatformat-full_format.ads/.adb` — formats all sources in a project tree
- `gnatformat-range_format.ads/.adb` — formats a selection range within one source file
- `gnatformat-abstract_writers.ads` — writer interface (write formatted output)
- `gnatformat-console_writers.ads/.adb` — writer that outputs to stdout (`--pipe` mode)
- `gnatformat-file_writers.ads/.adb` — writer that overwrites files in place
- `gitdiff.ads/.adb` — support for `--git-diff` mode (format only changed lines)

### Git subcommand wrapper (`src/formatters/git/`)

- `git_format.adb` — a small, dependency-free executable installed as
  `git-gnatformat` next to `gnatformat` on the `PATH`, so Git exposes it as the
  `git gnatformat` subcommand. It translates `git gnatformat [<base-commit>]
  [<extra args>]` into `gnatformat --gitdiff <base-commit> [<extra args>]`
  (defaulting the base to `HEAD`), locates the sibling `gnatformat` binary,
  spawns it, and forwards its exit status. Kept separate from the driver so the
  wrapper stays ~1MB instead of being a ~57MB copy of the formatter; built by
  `git_gnatformat.gpr`.

### Configuration via GPR

Formatting options can be set in a project file under `package Format`:

```ada
package Format is
   for Width use "100";
   for Indentation use "4";
   for Indentation_Kind use "spaces";  -- or "tabs"
   for Indentation_Continuation use "2";
   for End_Of_Line use "lf";           -- or "crlf"
   for Charset use "utf-8";
   for Keyword_Casing use "lower";     -- or "upper" or "keep"
   for Ignore use "ignore.txt";
end Format;
```

Attributes can be indexed by language (`"Ada"`) or by individual source filename.

## Pre-commit Hooks

The repo uses pre-commit for auto-formatting. The local hook runs `gnatformat_edge` on `.ads`/`.adb`
files (excluding testsuite), and `black` on `.py` files. Install with `pre-commit install`.
