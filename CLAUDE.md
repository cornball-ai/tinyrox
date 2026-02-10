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
`@title`, `@description`, `@details`, `@param`, `@return`, `@value`, `@examples`, `@seealso`, `@references`, `@aliases`, `@keywords`, `@name`, `@rdname`, `@noRd`

### Namespace
`@export`, `@exportS3Method`, `@import`, `@importFrom`, `@useDynLib`

## What it does NOT do

- Markdown parsing
- `@inheritParams` or inheritance
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
