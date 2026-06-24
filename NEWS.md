# tinyrox 0.3.3.7

* `document()` now prunes stale Rd files for topics that were renamed or removed (#22). After regenerating, it deletes `man/*.Rd` pages the current run did not produce, but only files tinyrox owns (first line is the tinyrox marker); hand-written Rd and Rd from other tools are never touched. Pass `prune_rd = FALSE` for the previous keep-everything behaviour. The returned list gains a `pruned` element.

# tinyrox 0.3.3.6

* Accept `@returns` as a plural alias of `@return` (#24). roxygen2 supports both spellings, so an unlisted `@returns` no longer aborts `document()`.
* Unknown tags now warn and are skipped instead of aborting the run (roxygen2's behavior). One unlisted or misspelled tag no longer takes down `document()` for the whole package; the offending tag is named in the warning and its content is dropped, while every other tag still parses.
* Remove DESCRIPTION-field linting (#23). The Title/Description unquoted-name check (`check_description_cran()`, `fix_description_cran()`) and the web-service-link check are gone. A documentation generator should not lint DESCRIPTION prose; the checks leaned on hardcoded, opinionated name lists with no roxygen2 or `R CMD check` equivalent, and one of them flagged (and `fix = TRUE` would rewrite) the ordinary word "graphics" in "base R graphics system". The token-based code checker and example checks (`check_cran()`, `check_examples_cran()`) remain.

# tinyrox 0.3.3.5

* The CRAN code checker scans parse tokens (`utils::getParseData()`) instead of raw source lines (#20). Comments and string literals can no longer trigger findings, `torch.cat()` is no longer `cat()`, `print()`/`cat()` are allowed inside `print.*`/`format.*` S3 methods, and a local variable named `T` or `F` is no longer mistaken for the logical shorthand. `setwd()`/`on.exit()` pairing and `set.seed()` literals are judged within the enclosing function instead of a fixed line window. Unparseable files report one finding instead of erroring.

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
