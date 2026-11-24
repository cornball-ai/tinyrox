## Submission

This is a patch update to the CRAN release 0.4.1. It fixes cases where
documentation or NAMESPACE entries were dropped without an error, removes
two false positives from the package's own checks, and adds two tags.

* Rd file names are portable for every topic. Files named after S3 methods
  such as `$.foo` or `[[.foo` were excluded from the tarball by
  `R CMD build`.
* A replacement S3 method gets a valid `\usage` line.
* A bare `@exportS3Method`, and the `pkg::generic` form, now produce the
  `S3method()` directive; both were dropped or written incorrectly.
* Datasets documented with the quoted-name convention get an Rd page, and
  `@format` and `@source` are supported.
* `@examplesIf` is supported.
* The undocumented-parameter check accepts `@param a,b`, and the
  missing-examples check runs per Rd page rather than per block.
* `check_cran()` warns about markdown that reaches the Rd unchanged.

No exported function is removed or renamed and no signature changes. The
list returned by `check_cran()` gains a `markdown` element.

## Test environments

* local: Ubuntu 24.04, R 4.6.1 (`--as-cran --run-donttest`, with the PDF
  manual)
* GitHub Actions: ubuntu-latest, macos-latest

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

One reverse dependency on CRAN ('tinypkgr'). `R CMD check` of its CRAN
release 0.2.2 passes against this version; it calls `tinyrox::document()`,
whose interface is unchanged.
