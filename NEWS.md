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
