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
