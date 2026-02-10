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
