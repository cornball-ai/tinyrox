## Submission

This is a patch update to the CRAN release 0.4.0 with two Rd-generation
fixes.

* Overlong `\usage` lines now wrap in roxygen2's style (one argument per
  line, breaking inside a long default when needed), so generated pages no
  longer trigger the "Rd line widths" NOTE in `R CMD check`.
* An already-escaped `\%` in roxygen source is no longer double-escaped
  into `\\%`, which previously produced malformed Rd for packages migrated
  from roxygen2.

No exported interfaces change.

## Test environments

* local: Ubuntu 24.04, R 4.6.1
* GitHub Actions: ubuntu-latest, macos-latest
* Windows 10: R-devel and R 4.6.0

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

One reverse dependency on CRAN ('tinypkgr'). Its test suite passes against
this version; it calls `tinyrox::document()`, whose interface is unchanged.
