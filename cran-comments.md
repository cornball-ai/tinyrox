## Submission

This is an update to the CRAN release 0.3.3.

The notable change is the removal of two exported functions,
`check_description_cran()` and `fix_description_cran()`, which linted the
DESCRIPTION file. DESCRIPTION linting is out of scope for a documentation
generator, and the checks produced false positives (for example, flagging
the ordinary English word "graphics"). The package's only reverse
dependency on CRAN, 'tinypkgr', calls only `tinyrox::document()` and is
unaffected by the removal.

Other changes: `document()` now prunes stale Rd files for renamed or removed
topics (opt out with `prune_rd = FALSE`), accepts `@returns` as an alias of
`@return`, and warns-and-skips unknown tags instead of aborting; the CRAN
code checker is now token-based. See NEWS.md for the full list.

This update follows 0.3.3 fairly closely on purpose, to retire the
misfiring DESCRIPTION linter before more users hit its false positives.

## Test environments

* local: Ubuntu 24.04, R 4.x
* GitHub Actions: ubuntu-latest, macos-latest
* win-builder: R-devel and R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

One reverse dependency on CRAN ('tinypkgr'). It calls only
`tinyrox::document()`, whose interface is unchanged, so the removed exported
functions do not affect it.
