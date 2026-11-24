# tinyrox 0.4.2

Consolidates the 0.4.1.x development cycle: fixes for documentation and `NAMESPACE` entries that were dropped without an error, two false positives in the checks, and two additions (`@examplesIf` and dataset documentation).

* `@exportS3Method pkg::generic` writes `S3method(pkg::generic,class)`, R's delayed registration for a generic the package does not import. It used to drop the `pkg::` prefix, and the two-argument form quoted it; either way the method was never registered. The class is the object name with `generic.` removed, and a name that does not start that way warns.
* `@examplesIf cond` is supported (#39). It was an unknown tag, so its examples were skipped with a warning and the page shipped with none. The output matches roxygen2: the guard sits in `\dontshow{}` and the help page shows only the example code. Several example tags on one block now all reach the page, in the order written; a second `@examples` used to replace the first.
* A bare `@exportS3Method` registers the method (#37). With no argument the tag was dropped silently: no `S3method()` line and no export, so dispatch failed at runtime. Generic and class now come from the object name, a known or package-defined generic first (`as.data.frame.foo`), and a name with no dot to split on warns.
* `@param a,b desc` documents both formals for the undocumented-parameter check (#40). The Rd was already right (`\item{a,b}{...}`); the check compared the joined name against each formal and reported both as undocumented. A joined name also sorts at its earliest formal in `\arguments`.
* The missing-examples check runs per Rd page (#41). A function merged into a page through `@rdname` is no longer reported when another block on the page carries the examples, and a bare `@export` above an S3 method, which generates no page, is skipped.
* `check_cran()` warns when roxygen text contains markdown that ships verbatim (#42): backticked terms and `[fn()]` links, reported once per file. Example code, `@noRd` blocks, and text already inside `\code{}` are ignored. Nothing is converted. Silence it with `options(tinyrox.markdown_check = FALSE)`. The result list gains a `markdown` element.
* A replacement S3 method (`$<-.myclass`, `[[<-.myclass`, `dim<-.myclass`) was given the usage `\method{$<-}{myclass}(x, name, value)`, which `R CMD check` reports as a bad `\usage` line: the `<-` sits on the generic, not at the end of the name, so the replacement test never fired. It is `\method{$}{myclass}(x, name) <- value` now, as for a plain replacement function. Found in glinty's two output-registration methods.
* Datasets are documented. A block over a bare `"name"` line (the roxygen data convention) used to be dropped silently: no Rd, no warning, and `R CMD check` then reported the dataset undocumented. The directive now parses like the `"_PACKAGE"` sentinel, `@format` and `@source` are supported tags (an author's `@format` wins over the auto-introspected one), and data pages carry `\source{}`. Found standing up examen's `are` dataset.
* Rd file names are portable for every topic, not just `%op%` and a leading dot. `$.foo`, `[[.foo` and `$<-.foo` are ordinary S3 methods, and the files named after them were being **dropped from the tarball**: `R CMD build` prints "excluding invalid files" and carries on, and the `R CMD check` that follows reads a tarball those files are no longer in, so nothing fails. glinty shipped four undocumented S3 methods that way with every CI job green.
* The substitution table is roxygen2's, so a package moving between the two tools does not churn its `man` directory, and the two cases tinyrox already handled (`%||%`, a leading dot) come out spelled exactly as before. `document()` now refuses outright if a name it wrote would still be dropped.

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
