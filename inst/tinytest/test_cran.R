# Tests for CRAN compliance checking (code + example checks)

# Test find_dontrun_examples
test_dontrun <- function() {
  # Exported function with \dontrun should be flagged
  lines1 <- c(
    "#' Title",
    "#' @export",
    "#' @examples",
    "#' \\dontrun{",
    "#' foo()",
    "#' }",
    "foo <- function() 1"
  )
  expect_true("foo" %in% tinyrox:::find_dontrun_examples(lines1))

  # Exported function with \donttest should NOT be flagged
  lines2 <- c(
    "#' Title",
    "#' @export",
    "#' @examples",
    "#' \\donttest{",
    "#' bar()",
    "#' }",
    "bar <- function() 1"
  )
  expect_equal(length(tinyrox:::find_dontrun_examples(lines2)), 0)

  # Non-exported function with \dontrun should NOT be flagged
  lines3 <- c(
    "#' Title",
    "#' @examples",
    "#' \\dontrun{",
    "#' baz()",
    "#' }",
    "baz <- function() 1"
  )
  expect_equal(length(tinyrox:::find_dontrun_examples(lines3)), 0)
}
test_dontrun()

# Test find_long_example_lines
test_long_lines <- function() {
  short_line <- paste0("#' ", paste(rep("x", 50), collapse = ""))
  long_line <- paste0("#' ", paste(rep("x", 101), collapse = ""))

  # Long line in examples should be flagged
  lines1 <- c(
    "#' Title",
    "#' @examples",
    long_line,
    "foo <- function() 1"
  )
  expect_equal(length(tinyrox:::find_long_example_lines(lines1, "test.R")), 1)

  # Short line should not be flagged
  lines2 <- c(
    "#' Title",
    "#' @examples",
    short_line,
    "foo <- function() 1"
  )
  expect_equal(length(tinyrox:::find_long_example_lines(lines2, "test.R")), 0)

  # Long line outside @examples should not be flagged
  lines3 <- c(
    "#' Title",
    "#' @description",
    long_line,
    "#' @examples",
    "#' foo()",
    "foo <- function() 1"
  )
  expect_equal(length(tinyrox:::find_long_example_lines(lines3, "test.R")), 0)
}
test_long_lines()

# Build a throwaway package from named file contents
make_pkg <- function(files) {
  pkg <- tempfile("pkg")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  writeLines(c("Package: mypkg", "Title: Test", "Version: 0.0.1"),
             file.path(pkg, "DESCRIPTION"))
  for (f in names(files)) {
    writeLines(files[[f]], file.path(pkg, "R", f))
  }
  pkg
}

# Missing-examples check runs per topic, not per block (#41)
test_examples_per_topic <- function() {
  pkg <- make_pkg(list("f.R" = c(
    "#' Two grouped functions",
    "#'",
    "#' @param a,b Whole numbers.",
    "#' @return A number.",
    "#' @examples",
    "#' g(1, 2)",
    "#' @rdname grouped",
    "#' @export",
    "g <- function(a, b) a + b",
    "",
    "#' @rdname grouped",
    "#' @export",
    "h <- function(a, b) a - b",
    "",
    "#' @export",
    "print.foo <- function(x, ...) cat(\"foo\\n\")",
    "",
    "#' No examples here",
    "#' @export",
    "bare <- function() 1"
  )))
  on.exit(unlink(pkg, recursive = TRUE))

  blocks <- tinyrox:::parse_package(pkg)
  expect_equal(tinyrox:::find_exports_without_examples(blocks), "bare")
  expect_warning(res <- tinyrox::check_examples_cran(pkg),
                 "without examples: bare$")
  expect_equal(res, "bare")

  # Comma-separated @param names document each formal (#40); the only
  # undocumented-parameter warning here would be a false one.
  w <- character()
  withCallingHandlers(
    tinyrox::document(pkg, cran_check = TRUE, silent = TRUE),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    },
    message = function(cnd) invokeRestart("muffleMessage")
  )
  expect_false(any(grepl("Undocumented parameters", w)))
  rd <- readLines(file.path(pkg, "man", "grouped.Rd"))
  expect_true(any(grepl("\\item{a,b}{Whole numbers.}", rd, fixed = TRUE)))
}
test_examples_per_topic()

# Markdown that ships verbatim is reported once per file (#42)
test_markdown <- function() {
  lines <- c(
    "#' Title with `code`",              # 1: flagged
    "#'",
    "#' See [other_fn()] for more.",     # 3: flagged
    "#' @param x A \\code{`[`} method.", # 4: inside \code{}, fine
    "#' @param y Index x[i] directly.",  # 5: plain brackets, fine
    "#' @examples",
    "#' `%||%`(1, 2)",                   # 7: example code, fine
    "#' @export",
    "f <- function(x, y) 1",
    "",
    "#' Hidden `thing`",                 # 11: noRd block, fine
    "#' @noRd",
    "g <- function() 1"
  )
  expect_equal(tinyrox:::find_markdown_lines(lines), c(1L, 3L))
  expect_equal(tinyrox:::find_markdown_lines("x <- 1"), integer())

  pkg <- make_pkg(list("f.R" = lines, "clean.R" = c(
    "#' Clean",
    "#' @export",
    "k <- function() 1"
  )))
  on.exit(unlink(pkg, recursive = TRUE))

  expect_warning(res <- tinyrox:::check_markdown_rd(pkg),
                 "f.R: 2 line\\(s\\), first at f.R:1")
  expect_equal(names(res), "f.R")

  old <- options(tinyrox.markdown_check = FALSE)
  on.exit(options(old), add = TRUE)
  expect_silent(res_off <- tinyrox:::check_markdown_rd(pkg))
  expect_equal(length(res_off), 0)
}
test_markdown()
