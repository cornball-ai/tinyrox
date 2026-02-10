# Tests for @rdname support

# --- Test 1: Two functions sharing @rdname produce single merged Rd ---

blocks_shared <- list(
  list(
    lines = c("Get a value", "", "Retrieves the stored value.", "@param x An object.",
               "@return The value.", "@rdname getset", "@export"),
    object = "get_value",
    type = "function",
    formals = list(names = "x", usage = "x"),
    file = "test.R",
    line = 1
  ),
  list(
    lines = c("@param x An object.", "@param value New value to set.",
               "@rdname getset", "@export"),
    object = "set_value",
    type = "function",
    formals = list(names = c("x", "value"), usage = c("x", "value")),
    file = "test.R",
    line = 10
  )
)

tags1 <- tinyrox:::parse_tags(blocks_shared[[1]]$lines, "get_value")
tags2 <- tinyrox:::parse_tags(blocks_shared[[2]]$lines, "set_value")

# Verify @rdname parsed correctly
expect_equal(tags1$rdname, "getset")
expect_equal(tags2$rdname, "getset")

# Generate merged Rd
entries <- list(
  list(tags = tags1, block = blocks_shared[[1]]),
  list(tags = tags2, block = blocks_shared[[2]])
)
rd <- tinyrox:::generate_rd_grouped("getset", entries, list())

# Should have \name{getset}
expect_true(grepl("\\\\name\\{getset\\}", rd))

# Should have aliases for both functions
expect_true(grepl("\\\\alias\\{get_value\\}", rd))
expect_true(grepl("\\\\alias\\{set_value\\}", rd))

# Title/description from primary (first block since neither name matches "getset")
expect_true(grepl("\\\\title\\{Get a value\\}", rd))
expect_true(grepl("Retrieves the stored value", rd))

# Usage should contain both functions
expect_true(grepl("get_value\\(x\\)", rd))
expect_true(grepl("set_value\\(x, value\\)", rd))

# Merged params: x from get_value (first wins), value from set_value
expect_true(grepl("\\\\item\\{x\\}", rd))
expect_true(grepl("\\\\item\\{value\\}", rd))

# Return from primary
expect_true(grepl("The value", rd))


# --- Test 2: Primary block is the one whose name matches the topic ---

blocks_primary <- list(
  list(
    lines = c("@param x Input.", "@rdname myfuns", "@export"),
    object = "helper_fun",
    type = "function",
    formals = list(names = "x", usage = "x"),
    file = "test.R",
    line = 1
  ),
  list(
    lines = c("My Functions", "", "The main family of functions.",
               "@param x Input.", "@return Something.", "@rdname myfuns", "@export"),
    object = "myfuns",
    type = "function",
    formals = list(names = "x", usage = "x"),
    file = "test.R",
    line = 10
  )
)

tags_h <- tinyrox:::parse_tags(blocks_primary[[1]]$lines, "helper_fun")
tags_m <- tinyrox:::parse_tags(blocks_primary[[2]]$lines, "myfuns")

entries2 <- list(
  list(tags = tags_h, block = blocks_primary[[1]]),
  list(tags = tags_m, block = blocks_primary[[2]])
)
rd2 <- tinyrox:::generate_rd_grouped("myfuns", entries2, list())

# Primary is myfuns (name matches topic), so title/description come from it
expect_true(grepl("\\\\title\\{My Functions\\}", rd2))
expect_true(grepl("The main family of functions", rd2))
expect_true(grepl("\\\\value\\{", rd2))


# --- Test 3: @rdname + @export still generates correct NAMESPACE entries ---

ns <- tinyrox:::generate_namespace(blocks_shared)

# Both functions should be exported independently
expect_true(grepl("export\\(get_value\\)", ns))
expect_true(grepl("export\\(set_value\\)", ns))


# --- Test 4: Single block without @rdname works identically (regression) ---

blocks_single <- list(
  list(
    lines = c("Solo function", "", "Does something alone.",
               "@param x Input.", "@return Output.", "@export"),
    object = "solo_fun",
    type = "function",
    formals = list(names = "x", usage = "x"),
    file = "test.R",
    line = 1
  )
)

tags_solo <- tinyrox:::parse_tags(blocks_single[[1]]$lines, "solo_fun")
expect_null(tags_solo$rdname)

rd_solo <- tinyrox:::generate_rd(tags_solo, blocks_single[[1]]$formals)
expect_true(grepl("\\\\name\\{solo_fun\\}", rd_solo))
expect_true(grepl("\\\\title\\{Solo function\\}", rd_solo))


# --- Test 5: generate_all_rd groups by @rdname ---

if (at_home()) {
  tmp <- tempdir()
  pkg <- file.path(tmp, "rdnamepkg")
  dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg, "man"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Package: rdnamepkg", "Title: Test", "Version: 0.1",
                "Description: Test pkg."), file.path(pkg, "DESCRIPTION"))

  writeLines(c(
    "#' Get a value",
    "#'",
    "#' Retrieves the stored value.",
    "#' @param x An object.",
    "#' @return The value.",
    "#' @rdname getset",
    "#' @export",
    "get_value <- function(x) x",
    "",
    "#' @param x An object.",
    "#' @param value New value.",
    "#' @rdname getset",
    "#' @export",
    "set_value <- function(x, value) value"
  ), file.path(pkg, "R", "getset.R"))

  # Also add a standalone function (no @rdname)
  writeLines(c(
    "#' Standalone",
    "#'",
    "#' A standalone function.",
    "#' @param z Input.",
    "#' @export",
    "standalone <- function(z) z"
  ), file.path(pkg, "R", "standalone.R"))

  # Run document()
  tinyrox::document(pkg)

  # Should produce getset.Rd (merged) and standalone.Rd (single)
  rd_files <- list.files(file.path(pkg, "man"), pattern = "\\.Rd$")
  expect_true("getset.Rd" %in% rd_files)
  expect_true("standalone.Rd" %in% rd_files)
  # Should NOT produce separate get_value.Rd or set_value.Rd
  expect_false("get_value.Rd" %in% rd_files)
  expect_false("set_value.Rd" %in% rd_files)

  # Check merged Rd content
  rd_content <- paste(readLines(file.path(pkg, "man", "getset.Rd")), collapse = "\n")
  expect_true(grepl("\\\\alias\\{get_value\\}", rd_content))
  expect_true(grepl("\\\\alias\\{set_value\\}", rd_content))
  expect_true(grepl("get_value\\(x\\)", rd_content))
  expect_true(grepl("set_value\\(x, value\\)", rd_content))

  # Check NAMESPACE has both exports
  ns_content <- paste(readLines(file.path(pkg, "NAMESPACE")), collapse = "\n")
  expect_true(grepl("export\\(get_value\\)", ns_content))
  expect_true(grepl("export\\(set_value\\)", ns_content))
  expect_true(grepl("export\\(standalone\\)", ns_content))

  # Cleanup
  unlink(pkg, recursive = TRUE)
}


# --- Test 6: Details and examples are concatenated from all blocks ---

blocks_concat <- list(
  list(
    lines = c("Topic funcs", "", "Description here.",
               "@details Detail A.", "@examples", "func_a(1)",
               "@rdname topic_funcs", "@export"),
    object = "func_a",
    type = "function",
    formals = list(names = "x", usage = "x"),
    file = "test.R",
    line = 1
  ),
  list(
    lines = c("@details Detail B.", "@examples", "func_b(2)",
               "@rdname topic_funcs", "@export"),
    object = "func_b",
    type = "function",
    formals = list(names = "y", usage = "y"),
    file = "test.R",
    line = 10
  )
)

tags_a <- tinyrox:::parse_tags(blocks_concat[[1]]$lines, "func_a")
tags_b <- tinyrox:::parse_tags(blocks_concat[[2]]$lines, "func_b")

entries_c <- list(
  list(tags = tags_a, block = blocks_concat[[1]]),
  list(tags = tags_b, block = blocks_concat[[2]])
)
rd_c <- tinyrox:::generate_rd_grouped("topic_funcs", entries_c, list())

# Details from both blocks
expect_true(grepl("Detail A", rd_c))
expect_true(grepl("Detail B", rd_c))

# Examples from both blocks
expect_true(grepl("func_a\\(1\\)", rd_c))
expect_true(grepl("func_b\\(2\\)", rd_c))
