# Tests for tags.R

# Test basic tag parsing
lines <- c(
  "Add Two Numbers",
  "",
  "@param x First number",
  "@param y Second number",
  "@return The sum",
  "@export"
)

tags <- tinyrox:::parse_tags(lines, "add")

expect_equal(tags$title, "Add Two Numbers")
expect_equal(tags$name, "add")
expect_true(tags$export)
expect_equal(tags$params$x, "First number")
expect_equal(tags$params$y, "Second number")
expect_equal(tags$return, "The sum")

# Test @noRd
lines_noRd <- c("Internal function", "@noRd")
tags_noRd <- tinyrox:::parse_tags(lines_noRd, "internal")
expect_true(tags_noRd$noRd)

# Test @keywords
lines_kw <- c("Title", "@keywords internal")
tags_kw <- tinyrox:::parse_tags(lines_kw, "foo")
expect_equal(tags_kw$keywords, "internal")

# Test @aliases
lines_alias <- c("Title", "@aliases foo bar baz")
tags_alias <- tinyrox:::parse_tags(lines_alias, "main")
expect_equal(tags_alias$aliases, c("foo", "bar", "baz"))

# Test multiline @description
lines_desc <- c(
  "Title",
  "@description This is a",
  "multiline description",
  "with three lines",
  "@export"
)
tags_desc <- tinyrox:::parse_tags(lines_desc, "foo")
expect_true(grepl("multiline", tags_desc$description))
expect_true(grepl("three lines", tags_desc$description))

# Unknown tag warns and is skipped (roxygen2 behavior), not a hard error
expect_warning(
  tinyrox:::parse_tags(c("@unknowntag value"), "foo"),
  pattern = "Unknown tag"
)

# A skipped unknown tag does not take down surrounding tags
tags_skip <- suppressWarnings(
  tinyrox:::parse_tags(c("Title", "@bogus nope", "@return The value"), "foo")
)
expect_equal(tags_skip$title, "Title")
expect_equal(tags_skip$return, "The value")

# Test @importFrom
lines_import <- c("Title", "@importFrom stats lm glm")
tags_import <- tinyrox:::parse_tags(lines_import, "foo")
expect_equal(length(tags_import$importFroms), 1)
expect_equal(tags_import$importFroms[[1]]$pkg, "stats")
expect_equal(tags_import$importFroms[[1]]$symbols, c("lm", "glm"))

# Pre-tag content splits into title / description / details on blank lines
lines_tdd <- c(
  "Title Line",
  "",
  "Description paragraph.",
  "",
  "Details paragraph one.",
  "",
  "Details paragraph two."
)
tags_tdd <- tinyrox:::parse_tags(lines_tdd, "foo")
expect_equal(tags_tdd$title, "Title Line")
expect_equal(tags_tdd$description, "Description paragraph.")
expect_equal(tags_tdd$details, "Details paragraph one.\n\nDetails paragraph two.")

# Two paragraphs = title + description, no details
lines_td <- c("Title", "", "Description.")
tags_td <- tinyrox:::parse_tags(lines_td, "foo")
expect_equal(tags_td$title, "Title")
expect_equal(tags_td$description, "Description.")
expect_null(tags_td$details)

# One paragraph = title only; description falls back to title (existing behavior)
lines_t <- c("Just a title.")
tags_t <- tinyrox:::parse_tags(lines_t, "foo")
expect_equal(tags_t$title, "Just a title.")
expect_equal(tags_t$description, "Just a title.")
expect_null(tags_t$details)

# Explicit @details overrides paragraph-3 inference
lines_explicit <- c(
  "Title", "", "Description.", "", "Inferred details.",
  "@details Explicit details."
)
tags_explicit <- tinyrox:::parse_tags(lines_explicit, "foo")
expect_equal(tags_explicit$details, "Explicit details.")

# Multi-line description (no blank line between) stays as description
lines_multiline_desc <- c(
  "Title", "", "Line one of desc.", "Line two of desc.", "",
  "Now details."
)
tags_md <- tinyrox:::parse_tags(lines_multiline_desc, "foo")
expect_true(grepl("Line one", tags_md$description))
expect_true(grepl("Line two", tags_md$description))
expect_equal(tags_md$details, "Now details.")

# @returns is accepted as a plural alias of @return (roxygen2 supports both) (#24)
lines_returns <- c("Title", "@returns The result value")
tags_returns <- tinyrox:::parse_tags(lines_returns, "foo")
expect_equal(tags_returns$return, "The result value")

# @returns and @return land in the same slot
lines_return <- c("Title", "@return The result value")
tags_return <- tinyrox:::parse_tags(lines_return, "foo")
expect_equal(tags_returns$return, tags_return$return)

# @examplesIf wraps the examples in a hidden guard, as roxygen2 does (#39)
lines_if <- c("Title", "@examplesIf requireNamespace(\"stats\", quietly = TRUE)",
              "f(1)", "", "@export")
tags_if <- tinyrox:::parse_tags(lines_if, "f")
expect_equal(tags_if$examples, paste(
  "\\dontshow{if (requireNamespace(\"stats\", quietly = TRUE)) withAutoprint(\\{ # examplesIf}",
  "f(1)",
  "\\dontshow{\\}) # examplesIf}",
  sep = "\n"))
expect_true(tags_if$export)

# The generated page is valid Rd and keeps the example
rd_if <- tinyrox:::generate_rd(tags_if, list(names = "x", usage = "x"))
rd_if_file <- tempfile(fileext = ".Rd")
writeLines(rd_if, rd_if_file)
expect_equal(length(tools::checkRd(rd_if_file)), 0L)
expect_true(grepl("\\examples{", rd_if, fixed = TRUE))
unlink(rd_if_file)

# Plain and conditional examples on one block both reach the page, in order
lines_mix <- c("Title", "@examples", "a()", "@examplesIf interactive()", "b()")
tags_mix <- tinyrox:::parse_tags(lines_mix, "f")
expect_true(grepl("^a\\(\\)\n\\\\dontshow\\{if \\(interactive\\(\\)\\)",
                  tags_mix$examples))
expect_true(grepl("b()", tags_mix$examples, fixed = TRUE))

# No condition: warn and keep the examples
expect_warning(tags_nocond <- tinyrox:::parse_tags(
  c("Title", "@examplesIf", "f(1)"), "f"), "no condition")
expect_equal(tags_nocond$examples, "f(1)")
