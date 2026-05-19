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
