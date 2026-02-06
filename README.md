# tinyrox

Minimal R documentation generator - base R only, no magic.

## What it does

tinyrox is a lightweight alternative to roxygen2 that generates valid `.Rd` files and `NAMESPACE` from `#'` comments using only base R.

## Installation

```r
remotes::install_github("cornball-ai/tinyrox")
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
| `@example path` | Include example from file |
| `@seealso` | Cross-references |
| `@references` | Citations |
| `@section Title:` | Custom section |
| `@author` | Author information |
| `@family name` | Related functions |
| `@aliases` | Additional topic aliases |
| `@keywords` | Rd keywords (e.g., `internal`) |
| `@name` | Explicit topic name |
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

## CRAN Compliance Checking

tinyrox includes automated CRAN compliance checks:

```r
# Check DESCRIPTION for common issues
check_description_cran()
# Warns about: unquoted package names, missing web service links

# Check R code for CRAN policy violations
check_code_cran()
# Warns about: T/F, print()/cat(), .GlobalEnv, installed.packages(), etc.

# Run all checks
check_cran()

# Auto-fix DESCRIPTION quoting issues
fix_description_cran()
```

Issues detected:
- Unquoted package/software names in Title/Description
- Missing web service links for packages like hfhub, gh
- `T`/`F` instead of `TRUE`/`FALSE`
- `print()`/`cat()` instead of `message()`
- `installed.packages()` usage
- `.GlobalEnv` modifications
- `setwd()` without `on.exit()` restoration
- Hardcoded `set.seed()` without parameter

## Philosophy

tinyrox follows the [tinyverse](https://www.tinyverse.org) philosophy:

> Dependencies have real costs. Each dependency is an invitation to break your project.

**Design principles:**
- Minimize dependencies (tinyrox has none)
- Explicit over implicit - no inference magic
- Strict subset of tags - not everything roxygen2 does
- Deterministic output - same input = same output
- Fail fast on unknown tags

**What tinyrox does NOT do:**
- Markdown parsing
- Automatic dependency inference
- `@rdname` grouping magic
- pkgdown integration

## Development Workflow

tinyrox is part of the tinyverse toolchain for R package development:

| Package | Purpose |
|---------|---------|
| **tinyrox** | Documentation & NAMESPACE |
| **tinypkgr** | install, load_all, check, build |
| **tinytest** | Unit testing |
| **fyi** | Rd to markdown (LLM context, static sites) |

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
