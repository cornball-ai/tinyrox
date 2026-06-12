# Tests for check_code.R (token-based CRAN code checks)

check_src <- function(lines) {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  writeLines(lines, tmp)
  tinyrox:::check_code_file(tmp)
}

messages_of <- function(issues) {
  vapply(issues, function(x) x$message, character(1))
}

# --- T/F shorthand ---

# T used as logical shorthand is flagged
issues <- check_src("f <- function(x) sum(x, na.rm = T)")
expect_equal(length(issues), 1)
expect_true(grepl("TRUE/FALSE", issues[[1]]$message))

# Regression (#20): T/F inside comments are not flagged
issues <- check_src(c(
  "f <- function(x) {",
  "    # output is (B, T, dim)",
  "    x  # F is for fast",
  "}"
))
expect_equal(length(issues), 0)

# Regression (#20): T/F inside string literals are not flagged
issues <- check_src('f <- function(x) paste("T", \'F\', x)')
expect_equal(length(issues), 0)

# Regression (#20): a local variable named T is not the logical shorthand
issues <- check_src(c(
  "f <- function(x) {",
  "    T <- x$size(3)",
  "    seq_len(T)",
  "}"
))
expect_equal(length(issues), 0)

# T as a formal or for-loop variable is also a variable
issues <- check_src(c(
  "f <- function(T) T + 1",
  "g <- function(x) {",
  "    for (F in x) print(F)",
  "}"
))
expect_equal(length(issues), 1) # only the print() finding
expect_true(grepl("print\\(\\)/cat\\(\\)", issues[[1]]$message))

# A local T in one function does not excuse shorthand T in another
issues <- check_src(c(
  "f <- function(x) {",
  "    T <- 1",
  "    x + T",
  "}",
  "g <- function(x) sum(x, na.rm = T)"
))
expect_equal(length(issues), 1)
expect_equal(issues[[1]]$line, 5)

# $T and @T member access are not flagged
issues <- check_src("f <- function(x) x$T + x@F")
expect_equal(length(issues), 0)

# --- print()/cat() ---

# Regression (#20): cat() inside a print.* S3 method is fine
issues <- check_src(c(
  "print.myclass <- function(x, ...) {",
  '    cat("a myclass object\\n")',
  "    invisible(x)",
  "}"
))
expect_equal(length(issues), 0)

# format.* methods too
issues <- check_src(c(
  "format.myclass <- function(x, ...) {",
  '    cat("formatted\\n")',
  "}"
))
expect_equal(length(issues), 0)

# cat() in a regular function is still flagged
issues <- check_src(c(
  "describe <- function(x) {",
  '    cat("x is", x, "\\n")',
  "}"
))
expect_equal(length(issues), 1)
expect_equal(issues[[1]]$line, 2)

# Regression (#20): cat( inside a string literal is not a call
issues <- check_src(c(
  "make_script <- function(dim) {",
  '    sprintf("y = torch.cat([a, b], dim=%d)\\n", dim)',
  "}"
))
expect_equal(length(issues), 0)

# Regression (#20): a dotted name like torch.cat() is not base cat()
issues <- check_src("f <- function(a, b) torch.cat(a, b)")
expect_equal(length(issues), 0)

# --- other checks survive the rewrite ---

issues <- check_src("f <- function() installed.packages()")
expect_equal(length(issues), 1)
expect_true(grepl("installed.packages", issues[[1]]$message))

issues <- check_src('f <- function() assign("x", 1, envir = .GlobalEnv)')
expect_equal(length(issues), 1)
expect_true(grepl("GlobalEnv", issues[[1]]$message))

issues <- check_src("f <- function() options(warn = -1)")
expect_equal(length(issues), 1)
expect_true(grepl("suppressWarnings", issues[[1]]$message))

issues <- check_src("f <- function() options(warn = 2)")
expect_equal(length(issues), 0)

# setwd() without on.exit() is flagged
issues <- check_src(c(
  "f <- function(d) {",
  "    setwd(d)",
  "}"
))
expect_equal(length(issues), 1)
expect_true(grepl("on.exit", issues[[1]]$message))

# setwd() restored with on.exit() is fine, even more than 5 lines away
# (the old line-window check missed this)
issues <- check_src(c(
  "f <- function(d) {",
  "    old <- getwd()",
  "    x1 <- 1", "    x2 <- 2", "    x3 <- 3", "    x4 <- 4",
  "    x5 <- 5", "    x6 <- 6", "    x7 <- 7", "    x8 <- 8",
  "    setwd(d)",
  "    on.exit(setwd(old))",
  "}"
))
expect_equal(length(issues), 0)

# Hardcoded set.seed() is flagged
issues <- check_src("f <- function(x) { set.seed(42); sample(x) }")
expect_equal(length(issues), 1)
expect_true(grepl("set.seed", issues[[1]]$message))

# set.seed() in a function with a seed formal is fine
issues <- check_src(c(
  "f <- function(x, seed = 42) {",
  "    set.seed(42)",
  "    sample(x)",
  "}"
))
expect_equal(length(issues), 0)

# set.seed(seed) with a non-literal argument is fine
issues <- check_src("f <- function(x, s) { set.seed(s); sample(x) }")
expect_equal(length(issues), 0)

# --- file handling ---

# A file that does not parse reports one issue instead of erroring
issues <- check_src("f <- function( {")
expect_equal(length(issues), 1)
expect_true(grepl("does not parse", issues[[1]]$message))

# Issues come back sorted by line
issues <- check_src(c(
  "g <- function(x) sum(x, na.rm = F)",
  "h <- function() installed.packages()"
))
expect_equal(vapply(issues, function(x) x$line, numeric(1)), c(1, 2))
