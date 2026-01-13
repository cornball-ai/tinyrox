# Tests for rd.R

# Test escape_rd
expect_equal(rhydrogen:::escape_rd("hello"), "hello")
expect_equal(rhydrogen:::escape_rd("100%"), "100\\%")
expect_equal(rhydrogen:::escape_rd("{test}"), "\\{test\\}")
expect_equal(rhydrogen:::escape_rd("a\\b"), "a\\\\b")

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

rd <- rhydrogen:::generate_rd(tags, list(names = c("x", "y"), usage = c("x", "y")))

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
rd_alias <- rhydrogen:::generate_rd(tags_alias, list(names = c("x", "y"), usage = c("x", "y")))
expect_true(grepl("\\\\alias\\{plus\\}", rd_alias))
expect_true(grepl("\\\\alias\\{sum2\\}", rd_alias))

# Test with keywords
tags_kw <- tags
tags_kw$keywords <- c("internal", "math")
rd_kw <- rhydrogen:::generate_rd(tags_kw, list(names = c("x", "y"), usage = c("x", "y")))
expect_true(grepl("\\\\keyword\\{internal\\}", rd_kw))
expect_true(grepl("\\\\keyword\\{math\\}", rd_kw))
