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

# Regression (#18): blocks separated by a blank line must stay separate.
# Merging let an orphaned block's @export bleed into the next block, so a
# @noRd helper ended up in NAMESPACE.
tmp <- tempfile(fileext = ".R")
writeLines(c(
  "#' Orphaned block (its function was moved away)",
  "#' @param x A thing",
  "#' @export",
  "",
  "#' Internal helper",
  "#' @param text Text",
  "#' @noRd",
  ".split_chunks <- function(text) text"
), tmp)
expect_warning(blocks <- tinyrox:::parse_file(tmp),
               pattern = "not followed by an object definition")
expect_equal(length(blocks), 1)
expect_equal(blocks[[1]]$object, ".split_chunks")
tags <- tinyrox:::parse_tags(blocks[[1]]$lines, blocks[[1]]$object)
expect_false(tags$export)
expect_true(tags$noRd)
unlink(tmp)

# Plain # comments between a block and its function are fine (no orphan)
tmp <- tempfile(fileext = ".R")
writeLines(c(
  "#' Documented",
  "#' @param x A thing",
  "#' @export",
  "# implementation note",
  "# spanning two lines",
  "documented_fn <- function(x) x"
), tmp)
blocks <- tinyrox:::parse_file(tmp)
expect_equal(length(blocks), 1)
expect_equal(blocks[[1]]$object, "documented_fn")
unlink(tmp)

# A block at end of file warns instead of silently dropping its tags
tmp <- tempfile(fileext = ".R")
writeLines(c(
  "real_fn <- function(x) x",
  "",
  "#' @useDynLib mypkg, .registration = TRUE"
), tmp)
expect_warning(blocks <- tinyrox:::parse_file(tmp),
               pattern = "not followed by an object definition")
expect_equal(length(blocks), 0)
unlink(tmp)

# Regression: a multi-line signature longer than the old fixed 20-line lookahead
# must be captured in full (formals were silently truncated before).
long_args <- paste0("a", 1:30, " = NULL", collapse = ",\n  ")
big_src <- c("#' Big", "#' @export",
             paste0("big_fn <- function(\n  ", long_args, ") {"),
             "  NULL", "}")
tmp <- tempfile(fileext = ".R")
writeLines(big_src, tmp)
big <- Filter(function(o) identical(o$object, "big_fn"),
              tinyrox:::parse_file(tmp))[[1]]
expect_equal(length(big$formals$names), 30L)
expect_equal(big$formals$names[30], "a30")
expect_true(all(grepl("= NULL$", big$formals$usage)))
unlink(tmp)
