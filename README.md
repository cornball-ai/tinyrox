# rhydrogen

Minimal R documentation generator - base R only, no magic.

## What it does

rhydrogen is a lightweight alternative to roxygen2 that generates valid `.Rd` files and `NAMESPACE` from `#'` comments using only base R. No dependencies.

## Installation

```r
remotes::install_github("cornball-ai/rhydrogen")
```

## Usage

```r
library(rhydrogen)

# Generate docs and NAMESPACE
document()

# Install package (quiet by default)
install()

# Load for interactive development
load_all()

# Run tests
tinytest::test_package("mypkg")

# Validate generated files
check()

# Clean generated files
clean()
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
| `@aliases` | Additional topic aliases |
| `@keywords` | Rd keywords (e.g., `internal`) |
| `@name` | Explicit topic name |
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

## Philosophy

rhydrogen follows the [tinyverse](https://www.tinyverse.org) philosophy:

> Dependencies have real costs. Each dependency is an invitation to break your project.

**Design principles:**
- Base R only - no dependencies
- Explicit over implicit - no inference magic
- Strict subset of tags - not everything roxygen2 does
- Deterministic output - same input = same output
- Fail fast on unknown tags

**What rhydrogen does NOT do:**
- Markdown parsing
- Automatic dependency inference
- `@inheritParams` or other inheritance
- `@rdname` grouping magic
- pkgdown integration

## Development Workflow

```r
# Edit R/*.R files with #' comments

# Regenerate docs
rhydrogen::document()

# Test interactively
rhydrogen::load_all()

# Install and test
rhydrogen::install()
tinytest::test_package("mypkg")
```

## License

MIT
