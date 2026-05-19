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

# Test unknown tag error
expect_error(
  tinyrox:::parse_tags(c("@unknowntag value"), "foo"),
  pattern = "Unknown tag"
)

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
