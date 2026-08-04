# Tests for rd.R

# Test escape_rd
expect_equal(tinyrox:::escape_rd("hello"), "hello")
expect_equal(tinyrox:::escape_rd("100%"), "100\\%")
expect_equal(tinyrox:::escape_rd("{test}"), "\\{test\\}")
expect_equal(tinyrox:::escape_rd("a\\b"), "a\\b")

# Test generate_rd
tags <- list(
  title = "Add Numbers",
  description = "Adds two numbers together",
  details = NULL,
  params = list(x = "First number", y = "Second number"),
  return = "The sum",
  examples = "add(1, 2)",
  seealso = NULL,
  references = NULL,
  aliases = character(),
  keywords = character(),
  family = NULL,
  name = "add",
  noRd = FALSE
)

rd <- tinyrox:::generate_rd(tags, list(names = c("x", "y"), usage = c("x", "y")))

# Check required sections
expect_true(grepl("\\\\name\\{add\\}", rd))
expect_true(grepl("\\\\alias\\{add\\}", rd))
expect_true(grepl("\\\\title\\{Add Numbers\\}", rd))
# Description is now on separate line from opening brace
expect_true(grepl("\\\\description\\{", rd))
expect_true(grepl("Adds two numbers together", rd))

# Check optional sections
expect_true(grepl("\\\\arguments\\{", rd))
expect_true(grepl("\\\\item\\{x\\}", rd))
expect_true(grepl("\\\\item\\{y\\}", rd))
expect_true(grepl("\\\\value\\{", rd))
expect_true(grepl("\\\\examples\\{", rd))
expect_true(grepl("add\\(1, 2\\)", rd))

# Check usage for functions
expect_true(grepl("\\\\usage\\{", rd))
expect_true(grepl("add\\(x, y\\)", rd))

# Test with aliases
tags_alias <- tags
tags_alias$aliases <- c("plus", "sum2")
rd_alias <- tinyrox:::generate_rd(tags_alias, list(names = c("x", "y"), usage = c("x", "y")))
expect_true(grepl("\\\\alias\\{plus\\}", rd_alias))
expect_true(grepl("\\\\alias\\{sum2\\}", rd_alias))

# Test @section blocks are rendered (#10) - parse -> generate, the real path
sec_block <- c(
  "Example function",
  "",
  "Main description.",
  "",
  "@section Permissions:",
  "This text should appear in generated Rd.",
  "@export"
)
sec_tags <- tinyrox:::parse_tags(sec_block, "f")
expect_equal(length(sec_tags$sections), 1L)
rd_sec <- tinyrox:::generate_rd(sec_tags, list(names = character(), usage = ""))
expect_true(grepl("\\\\section\\{Permissions\\}\\{", rd_sec))
expect_true(grepl("This text should appear in generated Rd", rd_sec))

# Multi-word section title
sec_block2 <- c(
  "Title", "", "Desc.",
  "@section Special Permissions:",
  "Body text.",
  "@export"
)
sec_tags2 <- tinyrox:::parse_tags(sec_block2, "g")
rd_sec2 <- tinyrox:::generate_rd(sec_tags2, list(names = character(), usage = ""))
expect_true(grepl("\\\\section\\{Special Permissions\\}\\{", rd_sec2))

# Multiple @section blocks on one function, in order
sec_block3 <- c(
  "Title", "", "Desc.",
  "@section First:", "One.",
  "@section Second:", "Two.",
  "@export"
)
sec_tags3 <- tinyrox:::parse_tags(sec_block3, "h")
expect_equal(length(sec_tags3$sections), 2L)
rd_sec3 <- tinyrox:::generate_rd(sec_tags3, list(names = character(), usage = ""))
expect_true(grepl("\\\\section\\{First\\}\\{", rd_sec3))
expect_true(grepl("\\\\section\\{Second\\}\\{", rd_sec3))
expect_true(regexpr("First", rd_sec3) < regexpr("Second", rd_sec3))

# No @section -> no \section{} block leaks in
expect_false(grepl("\\\\section\\{", rd))

# Test with keywords
tags_kw <- tags
tags_kw$keywords <- c("internal", "math")
rd_kw <- tinyrox:::generate_rd(tags_kw, list(names = c("x", "y"), usage = c("x", "y")))
expect_true(grepl("\\\\keyword\\{internal\\}", rd_kw))
expect_true(grepl("\\\\keyword\\{math\\}", rd_kw))

# Test resolve_inherit_params
source_tags <- list(
  name = "base_func",
  params = list(
    x = "The x parameter",
    y = "The y parameter",
    z = "The z parameter"
  )
)

child_tags <- list(
  name = "child_func",
  params = list(y = "Overridden y param"),  # Already documented

  inheritParams = c("base_func")
)

all_tags <- list(base_func = source_tags, child_func = child_tags)
formals <- list(names = c("x", "y"), usage = c("x", "y"))  # Only has x and y

resolved <- tinyrox:::resolve_inherit_params(child_tags, all_tags, formals)

# Should inherit x (in formals, not documented)
expect_equal(resolved$params$x, "The x parameter")
# Should NOT override y (already documented)
expect_equal(resolved$params$y, "Overridden y param")
# Should NOT inherit z (not in formals)
expect_true(is.null(resolved$params$z))

# Test @name override suppresses formals from underlying function
# (e.g., @name pkg-package above .onLoad should not produce \usage)
test_name_override_no_usage <- function() {
  pkg <- file.path(tempdir(), "namepkg")
  dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines("Package: namepkg\nTitle: Test\nVersion: 0.1.0",
      file.path(pkg, "DESCRIPTION"))
  writeLines(c(
      "#' @name namepkg-package",
      "#' @title namepkg",
      "#' @description A test package.",
      "#' @useDynLib namepkg",
      ".onLoad <- function(libname, pkgname) {}"),
      file.path(pkg, "R", "zzz.R"))

  rd_files <- tinyrox:::generate_all_rd(
      tinyrox:::parse_package(pkg), pkg)

  rd_file <- file.path(pkg, "man", "namepkg-package.Rd")
  expect_true(file.exists(rd_file))
  rd_content <- paste(readLines(rd_file), collapse = "\n")

  # Should NOT have \usage with .onLoad formals
  expect_false(grepl("\\\\usage", rd_content))
  expect_false(grepl("libname", rd_content))

  unlink(pkg, recursive = TRUE)
}
test_name_override_no_usage()

# Test generate_package_rd uses base-R macros for title/description/author
test_package_rd_macros <- function() {
  pkg <- file.path(tempdir(), "macropkg")
  dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
      "Package: macropkg",
      "Title: A Macro-Style Package",
      "Version: 0.1.0",
      "Description: Demonstrates macro-based package Rd.",
      "Authors@R: person('Test', 'Dev', email='t@example.com', role=c('aut','cre'))"
    ), file.path(pkg, "DESCRIPTION"))
  writeLines(c(
      "#' macropkg: A Macro-Style Package",
      "#'",
      "#' Brief description here.",
      "#'",
      "#' Design notes paragraph one.",
      "#'",
      "#' Design notes paragraph two.",
      "#' @keywords internal",
      "\"_PACKAGE\""),
      file.path(pkg, "R", "macropkg-package.R"))

  tinyrox:::generate_all_rd(tinyrox:::parse_package(pkg), pkg)
  rd <- paste(readLines(file.path(pkg, "man", "macropkg-package.Rd")),
      collapse = "\n")

  # Uses macros, not hardcoded content
  expect_true(grepl("\\\\packageTitle\\{macropkg\\}", rd))
  expect_true(grepl("\\\\packageDescription\\{macropkg\\}", rd))
  expect_true(grepl("\\\\packageAuthor\\{macropkg\\}", rd))
  expect_true(grepl("\\\\packageMaintainer\\{macropkg\\}", rd))
  expect_true(grepl("\\\\packageIndices\\{macropkg\\}", rd))

  # Hand-written details land in \details{}
  expect_true(grepl("\\\\details\\{", rd))
  expect_true(grepl("Design notes paragraph one\\.", rd))
  expect_true(grepl("Design notes paragraph two\\.", rd))

  # User's title text in the R file should NOT be hardcoded into \title{}
  # (it comes from DESCRIPTION via \packageTitle)
  expect_false(grepl("\\\\title\\{macropkg: A Macro-Style Package\\}", rd))

  # keyword{internal} from @keywords still emitted
  expect_true(grepl("\\\\keyword\\{internal\\}", rd))

  unlink(pkg, recursive = TRUE)
}
test_package_rd_macros()

# Test package Rd without any details still works (no empty \details{})
test_package_rd_no_details <- function() {
  pkg <- file.path(tempdir(), "nodetailspkg")
  dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
      "Package: nodetailspkg", "Title: T", "Version: 0.1.0",
      "Description: D.",
      "Authors@R: person('A', 'B', email='a@b.c', role=c('aut','cre'))"
    ), file.path(pkg, "DESCRIPTION"))
  writeLines(c(
      "#' nodetailspkg: Short.",
      "#'",
      "#' Description only.",
      "#' @keywords internal",
      "\"_PACKAGE\""),
      file.path(pkg, "R", "nodetailspkg-package.R"))

  tinyrox:::generate_all_rd(tinyrox:::parse_package(pkg), pkg)
  rd <- paste(readLines(file.path(pkg, "man", "nodetailspkg-package.Rd")),
      collapse = "\n")

  # No \details{} block emitted when there's nothing to put in it
  expect_false(grepl("\\\\details\\{", rd))

  # Macros still present
  expect_true(grepl("\\\\packageTitle\\{nodetailspkg\\}", rd))
  expect_true(grepl("\\\\packageIndices\\{nodetailspkg\\}", rd))

  unlink(pkg, recursive = TRUE)
}
test_package_rd_no_details()

# --- Issue #29: \usage lines must stay within the 90-char Rd limit ---

# Short usage stays on a single line
expect_equal(tinyrox:::format_usage("add", c("x", "y")), "add(x, y)")

# Long defaults wrap roxygen2-style: one arg per line, overlong args broken
long_args <- c(
  'provider = c("anthropic", "anthropic_claude", "openai", "moonshot", "openai_codex", "ollama")',
  'access_token = Sys.getenv("ANTHROPIC_CLAUDE_ACCESS_TOKEN", "")'
)
usage29 <- tinyrox:::format_usage("choose_provider", long_args)
expect_equal(usage29, paste(
  "choose_provider(",
  '  provider = c("anthropic", "anthropic_claude", "openai", "moonshot", "openai_codex",',
  '    "ollama"),',
  '  access_token = Sys.getenv("ANTHROPIC_CLAUDE_ACCESS_TOKEN", "")',
  ")",
  sep = "\n"))
expect_true(all(nchar(strsplit(usage29, "\n")[[1]]) <= 90))

# 80-char boundary matches roxygen2: bare call of 79 chars stays single
# line, 80 chars wraps
arg79 <- strrep("a", 76)
expect_equal(tinyrox:::format_usage("f", arg79), paste0("f(", arg79, ")"))
arg80 <- strrep("a", 77)
expect_equal(tinyrox:::format_usage("f", arg80), paste0("f(\n  ", arg80, "\n)"))

# S3 method width decision uses the bare name, not the \method markup
s3_args <- c("x", strrep("b", 60))
s3_usage <- tinyrox:::format_usage("print.myclass", s3_args)
expect_equal(s3_usage,
             paste0("\\method{print}{myclass}(x, ", strrep("b", 60), ")"))

# Replacement function wraps with the suffix after the closing paren
repl_args <- c("x", strrep("y", 80), "value")
repl_usage <- tinyrox:::format_usage("dim<-", repl_args)
expect_equal(repl_usage,
             paste0("dim(\n  x,\n  ", strrep("y", 80), "\n) <- value"))

# wrap_usage_arg: quoted strings are never broken
expect_equal(
  tinyrox:::wrap_usage_arg('  x = c("a b c d e f", "g h i", "j k l")',
                           width = 20L, indent = 4L),
  '  x =\n    c("a b c d e f",\n    "g h i",\n    "j k l")')

# wrap_usage_arg: short input returned unchanged
expect_equal(tinyrox:::wrap_usage_arg("  x = 1"), "  x = 1")

# End-to-end: generated Rd has no line wider than 90 chars
tags29 <- list(
  title = "Choose a provider",
  description = "Chooses a provider",
  details = NULL,
  params = list(provider = "Provider id.", access_token = "Token."),
  return = "NULL, invisibly.",
  examples = NULL,
  seealso = NULL,
  references = NULL,
  aliases = character(),
  keywords = character(),
  family = NULL,
  name = "choose_provider",
  noRd = FALSE
)
rd29 <- tinyrox:::generate_rd(tags29,
                              list(names = c("provider", "access_token"),
                                   usage = long_args))
expect_true(all(nchar(strsplit(rd29, "\n")[[1]]) <= 90))

# --- Issue #31: already-escaped \% must not be double-escaped ---

# Behavior table from the issue: only the pre-escaped rows change
expect_equal(tinyrox:::escape_rd("frame_%04d.png"), "frame_\\%04d.png")
expect_equal(tinyrox:::escape_rd("frame_\\%04d.png"), "frame_\\%04d.png")
expect_equal(tinyrox:::escape_rd("x %in% y"), "x \\%in\\% y")
expect_equal(tinyrox:::escape_rd("trailing \\%"), "trailing \\%")

# Mixed bare and pre-escaped in one string
expect_equal(tinyrox:::escape_rd("50% and \\%"), "50\\% and \\%")

# Markup-passthrough branch preserves pre-escaped % too
expect_equal(tinyrox:::escape_rd('\\code{"frame_\\%04d.png"}'),
             '\\code{"frame_\\%04d.png"}')
expect_equal(tinyrox:::escape_rd('\\code{"frame_%04d.png"}'),
             '\\code{"frame_\\%04d.png"}')

# Examples path goes through the same escape
tags31 <- list(
  title = "T", description = "D", details = NULL,
  params = list(), return = NULL,
  examples = 'sprintf("%d of \\%d", 1)',
  seealso = NULL, references = NULL,
  aliases = character(), keywords = character(), family = NULL,
  name = "fmt", noRd = FALSE
)
rd31 <- tinyrox:::generate_rd(tags31, list(names = character(),
                                           usage = character()))
expect_true(grepl('sprintf("\\%d of \\%d", 1)', rd31, fixed = TRUE))

# --- Issue #30 acceptance: _PACKAGE through the full document() flow ---
test_package_sentinel_document <- function() {
  pkg <- file.path(tempdir(), "sentpkg")
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
      "Package: sentpkg", "Title: T", "Version: 0.0.1",
      "Description: D.",
      "Authors@R: person('A', 'B', email='a@b.c', role=c('aut','cre'))"
    ), file.path(pkg, "DESCRIPTION"))
  writeLines(c(
      "#' sentpkg: Test Package.",
      "#'",
      "#' Ignored description paragraph.",
      "#'",
      "#' Details paragraph.",
      "#' @keywords internal",
      "\"_PACKAGE\""),
      file.path(pkg, "R", "sentpkg-package.R"))
  writeLines(c(
      "#' Identity",
      "#' @param x A value.",
      "#' @return x.",
      "#' @examples",
      "#' ident(1)",
      "#' @export",
      "ident <- function(x) x"),
      file.path(pkg, "R", "ident.R"))

  tinyrox::document(pkg, cran_check = FALSE, silent = TRUE)

  # Package topic generated with docType and both aliases (?sentpkg resolves)
  rd_path <- file.path(pkg, "man", "sentpkg-package.Rd")
  expect_true(file.exists(rd_path))
  rd <- readLines(rd_path)
  expect_true("\\docType{package}" %in% rd)
  expect_true("\\alias{sentpkg}" %in% rd)
  expect_true("\\alias{sentpkg-package}" %in% rd)
  expect_true("Details paragraph." %in% rd)

  # NAMESPACE holds the one real export and nothing for the sentinel
  ns <- readLines(file.path(pkg, "NAMESPACE"))
  expect_equal(grep("^export", ns, value = TRUE), "export(ident)")
  expect_false(any(grepl("_PACKAGE", ns, fixed = TRUE)))

  # Re-documenting is idempotent and prune_rd leaves the package topic alone
  res2 <- tinyrox::document(pkg, cran_check = FALSE, silent = TRUE,
                            prune_rd = TRUE)
  expect_equal(readLines(rd_path), rd)
  expect_equal(res2$pruned, character(0))
}
test_package_sentinel_document()
