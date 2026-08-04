# tinyrox 0.4.1

Two Rd-generation fixes.

* `\usage` lines now stay within the 90-character Rd limit (#29). Overlong signatures wrap roxygen2-style: one argument per line, breaking inside a long default when needed, so `R CMD check` no longer NOTEs "Rd line widths".
* An already-escaped `\%` in roxygen source is no longer double-escaped into `\\%` (#31). Both bare `%` and roxygen2-style pre-escaped `\%` now produce a single `\%` in the generated Rd.

# tinyrox 0.4.0

Consolidates the 0.3.3.x development cycle.

* **Breaking:** removed the DESCRIPTION-field linting, `check_description_cran()` and `fix_description_cran()`, along with the web-service-link check (#23). A documentation generator should not lint DESCRIPTION prose, and the checks produced false positives (e.g. flagging the ordinary word "graphics"). The token-based code checker and example checks (`check_cran()`, `check_examples_cran()`) remain, though `check_cran()`'s result list no longer carries a `description` element.
* `document()` gains stale-Rd pruning: after regenerating it removes `man/*.Rd` pages for renamed or deleted topics, but only files tinyrox owns (first line is the tinyrox marker); hand-written Rd is never touched. Pass `prune_rd = FALSE` to disable (#22).
* Accept `@returns` as a plural alias of `@return` (#24).
* Recognise ESS-style `##'` doc comments in addition to `#'`, matching roxygen2's `#+'` (Dirk Eddelbuettel, #27).
* Unknown tags now warn and are skipped instead of aborting the run (roxygen2's behavior); one misspelled or unlisted tag no longer takes down `document()` for the whole package.
* The CRAN code checker scans parse tokens (`utils::getParseData()`) instead of raw source lines, eliminating false positives from comments, strings, and look-alikes such as `torch.cat()` (#20).
* Documentation blocks must be strictly consecutive `#'` lines: an orphaned block's `@export` can no longer bleed into the next function (#18), and `document()` warns instead of silently dropping a hand-added NAMESPACE directive (#17).
* Render `@section` blocks in the Rd for ordinary functions and `@rdname` groups (#10).
* Fix a false "undocumented parameters" warning for functions documented via a sibling block in an `@rdname` group (#12).

# tinyrox 0.3.1

* Replace internal `utils:::.getHelpFile()` call with `tools::parse_Rd()` for CRAN compliance.
* Add `@return` to `clean()`, `@examples` to `parse_tags()`.
* Prepare for initial CRAN submission.

# tinyrox 0.3.0

* Generate Rd files and NAMESPACE from roxygen2-style comments using base R.
* Support for `@rdname` grouping of multiple functions into one Rd file.
* CRAN compliance checking with `check_cran()` and `fix_description_cran()`.
* Exported `parse_tags()` for programmatic access to parsed documentation.
