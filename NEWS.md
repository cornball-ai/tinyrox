# tinyrox 0.3.3.4

* Documentation blocks are now strictly consecutive `#'` lines, roxygen2-style. Blocks separated by blank lines no longer merge, so an orphaned block's `@export` can't bleed into the next function and export a `@noRd` helper (#18). Orphaned blocks warn instead of being silently dropped.
* `document()` warns when regenerating NAMESPACE drops a directive with no backing tag (e.g. a hand-added `useDynLib()` line), instead of silently discarding it (#17). `export()` and `S3method()` churn is exempt.
* Plain `#` comments between a block and its function no longer detach the documentation.

# tinyrox 0.3.3.3

* Render `@section` blocks in the Rd for ordinary functions and `@rdname` groups. They were parsed but only emitted for package-level docs, so a function's `@section` was silently dropped (#10).

# tinyrox 0.3.3.2

* Fix false "undocumented parameters" warning for functions documented via a sibling block in an `@rdname` group; the check is now group-wide. Also gate the warning on `cran_check` rather than `silent` (#12).

# tinyrox 0.3.1

* Replace internal `utils:::.getHelpFile()` call with `tools::parse_Rd()` for CRAN compliance.
* Add `@return` to `clean()`, `@examples` to `parse_tags()`.
* Prepare for initial CRAN submission.

# tinyrox 0.3.0

* Generate Rd files and NAMESPACE from roxygen2-style comments using base R.
* Support for `@rdname` grouping of multiple functions into one Rd file.
* CRAN compliance checking with `check_cran()` and `fix_description_cran()`.
* Exported `parse_tags()` for programmatic access to parsed documentation.
