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

tags <- rhydrogen:::parse_tags(lines, "add")

expect_equal(tags$title, "Add Two Numbers")
expect_equal(tags$name, "add")
expect_true(tags$export)
expect_equal(tags$params$x, "First number")
expect_equal(tags$params$y, "Second number")
expect_equal(tags$return, "The sum")

# Test @noRd
lines_noRd <- c("Internal function", "@noRd")
tags_noRd <- rhydrogen:::parse_tags(lines_noRd, "internal")
expect_true(tags_noRd$noRd)

# Test @keywords
lines_kw <- c("Title", "@keywords internal")
tags_kw <- rhydrogen:::parse_tags(lines_kw, "foo")
expect_equal(tags_kw$keywords, "internal")

# Test @aliases
lines_alias <- c("Title", "@aliases foo bar baz")
tags_alias <- rhydrogen:::parse_tags(lines_alias, "main")
expect_equal(tags_alias$aliases, c("foo", "bar", "baz"))

# Test multiline @description
lines_desc <- c(
  "Title",
  "@description This is a",
  "multiline description",
  "with three lines",
  "@export"
)
tags_desc <- rhydrogen:::parse_tags(lines_desc, "foo")
expect_true(grepl("multiline", tags_desc$description))
expect_true(grepl("three lines", tags_desc$description))

# Test unknown tag error
expect_error(
  rhydrogen:::parse_tags(c("@unknowntag value"), "foo"),
  pattern = "Unknown tag"
)

# Test @importFrom
lines_import <- c("Title", "@importFrom stats lm glm")
tags_import <- rhydrogen:::parse_tags(lines_import, "foo")
expect_equal(length(tags_import$importFroms), 1)
expect_equal(tags_import$importFroms[[1]]$pkg, "stats")
expect_equal(tags_import$importFroms[[1]]$symbols, c("lm", "glm"))
