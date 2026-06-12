# Tests for namespace.R

# Create mock blocks
blocks <- list(
  list(
    lines = c("Function A", "@export"),
    object = "func_a",
    type = "function",
    formals = NULL,
    file = "test.R",
    line = 1
  ),
  list(
    lines = c("Function B", "@export", "@importFrom stats lm"),
    object = "func_b",
    type = "function",
    formals = NULL,
    file = "test.R",
    line = 10
  ),
  list(
    lines = c("Internal", "@keywords internal"),
    object = "internal_fn",
    type = "function",
    formals = NULL,
    file = "test.R",
    line = 20
  )
)

# Test generate_namespace
ns <- tinyrox:::generate_namespace(blocks)

# Check header
expect_true(grepl("tinyrox says", ns))

# Check exports
expect_true(grepl("export\\(func_a\\)", ns))
expect_true(grepl("export\\(func_b\\)", ns))

# Internal function should NOT be exported
expect_false(grepl("export\\(internal_fn\\)", ns))

# Check importFrom
expect_true(grepl("importFrom\\(stats,lm\\)", ns))

# Test S3 method with explicit @exportS3Method
blocks_s3 <- list(
  list(
    lines = c("Print method", "@exportS3Method print myclass"),
    object = "print.myclass",
    type = "function",
    formals = list(names = c("x", "..."), usage = c("x", "...")),
    file = "test.R",
    line = 1
  )
)

ns_s3 <- tinyrox:::generate_namespace(blocks_s3)
expect_true(grepl("S3method\\(print,myclass\\)", ns_s3))

# Test S3 method auto-detection from @export + function name pattern
blocks_s3_auto <- list(
  list(
    lines = c("Print method", "@export"),
    object = "print.myclass",
    type = "function",
    formals = list(names = c("x", "..."), usage = c("x", "...")),
    file = "test.R",
    line = 1
  )
)

ns_s3_auto <- tinyrox:::generate_namespace(blocks_s3_auto)
# Should auto-detect as S3 method, not regular export
expect_true(grepl("S3method\\(print,myclass\\)", ns_s3_auto))
expect_false(grepl("export\\(print.myclass\\)", ns_s3_auto))

# Test operator S3 methods auto-detected from @export
blocks_ops <- list(
  list(
    lines = c("@export"),
    object = "+.torch_tensor",
    type = "function",
    formals = list(names = c("e1", "e2"), usage = c("e1", "e2")),
    file = "test.R",
    line = 1
  ),
  list(
    lines = c("@export"),
    object = "$.nn_module",
    type = "function",
    formals = list(names = c("x", "name"), usage = c("x", "name")),
    file = "test.R",
    line = 10
  ),
  list(
    lines = c("@export"),
    object = "%%.torch_tensor",
    type = "function",
    formals = list(names = c("e1", "e2"), usage = c("e1", "e2")),
    file = "test.R",
    line = 20
  ),
  list(
    lines = c("@export"),
    object = "$<-.nn_module",
    type = "function",
    formals = list(names = c("x", "name", "value"), usage = c("x", "name", "value")),
    file = "test.R",
    line = 30
  )
)

ns_ops <- tinyrox:::generate_namespace(blocks_ops)
# Operators should be detected as S3 methods with quoted generics
expect_true(grepl('S3method\\("\\+",torch_tensor\\)', ns_ops))
expect_true(grepl('S3method\\("\\$",nn_module\\)', ns_ops))
expect_true(grepl('S3method\\("%%",torch_tensor\\)', ns_ops))
expect_true(grepl('S3method\\("\\$<-",nn_module\\)', ns_ops))
# Should NOT be regular exports
expect_false(grepl("export", ns_ops))

# Test @useDynLib with .registration = TRUE (namespace-only block on NULL)
blocks_dynlib <- list(
  list(
    lines = c("@useDynLib Rtorch, .registration = TRUE"),
    object = ".namespace_only",
    type = "namespace_only",
    formals = NULL,
    file = "test.R",
    line = 1
  ),
  list(
    lines = c("A function", "@export"),
    object = "hello",
    type = "function",
    formals = NULL,
    file = "test.R",
    line = 5
  )
)

ns_dynlib <- tinyrox:::generate_namespace(blocks_dynlib)
expect_true(grepl("useDynLib\\(Rtorch, \\.registration = TRUE\\)", ns_dynlib))
expect_true(grepl("export\\(hello\\)", ns_dynlib))

# Test @useDynLib with package name only
blocks_dynlib_simple <- list(
  list(
    lines = c("@useDynLib mypkg"),
    object = ".namespace_only",
    type = "namespace_only",
    formals = NULL,
    file = "test.R",
    line = 1
  )
)

ns_dynlib_simple <- tinyrox:::generate_namespace(blocks_dynlib_simple)
expect_true(grepl("useDynLib\\(mypkg\\)", ns_dynlib_simple))

# Regression (#17): overwriting must warn when it drops a directive that has
# no backing tag (e.g. a hand-added useDynLib), instead of silently breaking
# the package's compiled code.
tmp_pkg <- tempfile()
dir.create(tmp_pkg)
writeLines(c(
  "# tinyrox says don't edit this manually, but it can't stop you!",
  "",
  "useDynLib(mypkg, .registration = TRUE)",
  "",
  "export(old_fn)",
  "export(hello)"
), file.path(tmp_pkg, "NAMESPACE"))

new_content <- "# tinyrox says don't edit this manually, but it can't stop you!\n\nexport(hello)"
expect_warning(tinyrox:::write_namespace(new_content, tmp_pkg),
               pattern = "useDynLib\\(mypkg, \\.registration = TRUE\\)")
# The overwrite still happens - the warning is loud, not blocking
expect_false(any(grepl("useDynLib", readLines(file.path(tmp_pkg, "NAMESPACE")))))

# Dropped export()/S3method() lines are normal churn - no warning
writeLines(c(
  "export(old_fn)",
  "S3method(print,myclass)",
  "export(hello)"
), file.path(tmp_pkg, "NAMESPACE"))
expect_silent(tinyrox:::write_namespace(new_content, tmp_pkg))

# A directive kept in the new content does not warn
writeLines(c(
  "useDynLib(mypkg, .registration = TRUE)",
  "export(hello)"
), file.path(tmp_pkg, "NAMESPACE"))
keep_content <- "export(hello)\n\nuseDynLib(mypkg, .registration = TRUE)"
expect_silent(tinyrox:::write_namespace(keep_content, tmp_pkg))

# Missing NAMESPACE (first document() run) does not warn
unlink(file.path(tmp_pkg, "NAMESPACE"))
expect_silent(tinyrox:::write_namespace(new_content, tmp_pkg))
unlink(tmp_pkg, recursive = TRUE)
