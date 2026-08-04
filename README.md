# tinyrox

Minimal R documentation generator - base R only, no magic.

## What it does

tinyrox is a lightweight alternative to roxygen2 that generates valid `.Rd` files and `NAMESPACE` from `#'` comments using only base R.

## Installation

```r
install.packages("tinyrox")
```

## Usage

```r
library(tinyrox)

# Generate man/*.Rd and NAMESPACE from R/*.R
document()

# Clean generated files (man/, NAMESPACE)
clean()

# Check for common CRAN policy issues
check_cran()
```

## Supported Tags

### Documentation

| Tag | Purpose |
|-----|---------|
| `@title` | One-line title |
| `@description` | Short description |
| `@details` | Longer description |
| `@param name` | Parameter documentation |
| `@return` | Return value |
| `@value` | Alias for `@return` |
| `@examples` | Code examples (verbatim) |
| `@seealso` | Cross-references |
| `@references` | Citations |
| `@section Title:` | Custom section (package-level docs only) |
| `@author` | Accepted for roxygen2 compatibility; ignored (authors come from `DESCRIPTION`) |
| `@aliases` | Additional topic aliases |
| `@keywords` | Rd keywords (e.g., `internal`) |
| `@name` | Explicit topic name |
| `@rdname` | Group multiple blocks into one Rd file |
| `@inheritParams fn` | Copy params from another function |
| `@noRd` | Skip Rd generation |

### Namespace

| Tag | Effect |
|-----|--------|
| `@export` | `export()` |
| `@exportS3Method generic class` | `S3method()` |
| `@import pkg` | `import()` |
| `@importFrom pkg sym1 sym2` | `importFrom()` |
| `@useDynLib pkg` | `useDynLib()` |

## Example

```r
#' Add Two Numbers
#'
#' @param x First number
#' @param y Second number
#' @return The sum
#' @export
#'
#' @examples
#' add(1, 2)
add <- function(x, y) {
  x + y
}
```

## Package-Level Documentation

Attach a block to the `"_PACKAGE"` sentinel (same convention as roxygen2) to
generate a `?pkgname` landing topic:

```r
#' mypkg: One-Line Title
#'
#' This paragraph is ignored; the help page pulls the description from
#' DESCRIPTION instead (see below).
#'
#' Paragraphs three and beyond (or @details) become the details
#' section: design notes, limitations, getting-started prose.
#' @keywords internal
"_PACKAGE"
```

This writes `man/mypkg-package.Rd` with package `docType` and aliases for
`mypkg` and `mypkg-package`, so `?mypkg` resolves. Title, description,
author, and maintainer come from `DESCRIPTION` at render time via the base-R
Rd macros (`\packageTitle{}`, `\packageDescription{}`, `\packageAuthor{}`,
`\packageMaintainer{}`). DESCRIPTION stays the single source of truth, and
the block's title and description paragraphs are not copied into the Rd. A
function index is generated with `\packageIndices{}`. The sentinel is not
exported and adds nothing to `NAMESPACE`.

## CRAN Compliance Checking

tinyrox includes automated CRAN compliance checks:

```r
# Check R code for CRAN policy violations
check_code_cran()
# Warns about: T/F, print()/cat(), .GlobalEnv, installed.packages(), etc.

# Check exported functions for missing examples and \dontrun overuse
check_examples_cran()

# Run all checks (code + examples)
check_cran()
```

Issues detected:
- `T`/`F` instead of `TRUE`/`FALSE`
- `print()`/`cat()` instead of `message()`
- `installed.packages()` usage
- `.GlobalEnv` modifications
- `setwd()` without `on.exit()` restoration
- Hardcoded `set.seed()` without parameter
- Exported functions without examples; `\dontrun{}` overuse

## Philosophy

tinyrox follows the [tinyverse](https://www.tinyverse.org) philosophy:

> Dependencies have real costs. Each dependency is an invitation to break your project.

**Design principles:**
- Minimize dependencies (tinyrox has none)
- Explicit over implicit - no inference magic
- Strict subset of tags - not everything roxygen2 does
- Deterministic output - same input = same output
- Warn and skip unknown tags (roxygen2 behavior), never abort the run

**What tinyrox does NOT do:**
- Markdown parsing
- Automatic dependency inference
- `@family` (use `@seealso`)
- pkgdown integration

## Development Workflow

tinyrox is part of the tinyverse toolchain for R package development:

| Package | Purpose |
|---------|---------|
| **tinyrox** | Documentation & NAMESPACE |
| **tinypkgr** | install, load_all, check, build |
| **tinytest** | Unit testing |
| **rformat** | Token-based code formatter |

```r
# Edit R/*.R files with #' comments

# Regenerate docs
tinyrox::document()

# Load for interactive development (no install)
tinypkgr::load_all()

# Install and reload in current session
tinypkgr::reload()
tinytest::test_package("mypkg")

# Full R CMD check
tinypkgr::check()
```

Or from the command line with littler:

```bash
r -e 'tinyrox::document(); tinypkgr::install(); tinytest::test_package("mypkg")'
```

## License

GPL-3
