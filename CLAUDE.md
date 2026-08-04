# tinyrox

Minimal R documentation generator - base R only, no dependencies.

## What it does

Generates `.Rd` files and `NAMESPACE` from `#'` comments. That's it.

## Exports

| Function | Purpose |
|----------|---------|
| `document()` | Parse R files, generate Rd + NAMESPACE |
| `clean()` | Remove generated Rd files |

## Usage

```r
tinyrox::document()
tinyrox::clean()
```

Or from CLI:
```bash
r -e 'tinyrox::document()'
```

## Supported Tags

### Documentation
`@title`, `@description`, `@details`, `@param`, `@return`, `@value`, `@examples`, `@seealso`, `@references`, `@aliases`, `@keywords`, `@name`, `@rdname`, `@noRd`, `@inheritParams`

### Namespace
`@export`, `@exportS3Method`, `@exportClass`, `@import`, `@importFrom`, `@useDynLib`

### Package-level docs
A block attached to the `"_PACKAGE"` sentinel generates `man/pkg-package.Rd`
(aliases `pkg` and `pkg-package`). Title/description/author/maintainer come
from DESCRIPTION via base-R Rd macros; block paragraphs 3+ (or `@details`)
fill `\details{}`. Nothing is added to NAMESPACE.

## What it does NOT do

- Markdown parsing
- `@family` (use `@seealso`)
- Automatic dependency inference
- pkgdown integration

## Header

Generated files have this header:
```
% tinyrox says don't edit this manually, but it can't stop you!
```

## Development utilities

For `install()`, `load_all()`, `check()`, etc. see:
- **tinypkgr** package
- **littler** scripts (`install.r`, `check.r`, `build.r`)
