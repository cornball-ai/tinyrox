# Tests for parse.R

# Create a temp file with documented function
tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)

writeLines(c(
  "#' Add Two Numbers",
  "#'",
  "#' @param x First number",
  "#' @param y Second number",
  "#' @return The sum",
  "#' @export",
  "add <- function(x, y) x + y",
  "",
  "#' Internal helper",
  "#' @keywords internal",
  ".helper <- function() NULL"
), tmp)

# Test parse_file
blocks <- tinyrox:::parse_file(tmp)
expect_equal(length(blocks), 2)

# Test first block
expect_equal(blocks[[1]]$object, "add")
expect_equal(blocks[[1]]$type, "function")
expect_equal(blocks[[1]]$formals$names, c("x", "y"))
expect_equal(blocks[[1]]$formals$usage, c("x", "y"))

# Test second block
expect_equal(blocks[[2]]$object, ".helper")
expect_equal(blocks[[2]]$type, "function")

# Test parse_formals_text - returns list with names and usage
result <- tinyrox:::parse_formals_text("x, y")
expect_equal(result$names, c("x", "y"))
expect_equal(result$usage, c("x", "y"))

result <- tinyrox:::parse_formals_text("x, y = 1")
expect_equal(result$names, c("x", "y"))
expect_equal(result$usage, c("x", "y = 1"))

result <- tinyrox:::parse_formals_text("")
expect_equal(result$names, character())

expect_equal(tinyrox:::parse_formals_text("...")$names, "...")
