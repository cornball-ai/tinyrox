# Tests for stale-Rd pruning during document() (issue #22)

pkg <- file.path(tempdir(), "prunepkg")
if (dir.exists(pkg)) unlink(pkg, recursive = TRUE)
dir.create(file.path(pkg, "R"), recursive = TRUE)
writeLines(c("Package: prunepkg", "Title: Test", "Version: 0.1.0"),
           file.path(pkg, "DESCRIPTION"))
writeLines(c("#' Foo", "#' @param x A number.", "#' @return x.", "#' @export",
             "foo <- function(x) x"),
           file.path(pkg, "R", "fns.R"))

# First run generates man/foo.Rd
tinyrox::document(pkg, namespace = "none", cran_check = FALSE, silent = TRUE)
expect_true(file.exists(file.path(pkg, "man", "foo.Rd")))

# Ownership check: tinyrox recognises its own header, not hand-written Rd
expect_true(tinyrox:::tinyrox_owns_rd(file.path(pkg, "man", "foo.Rd")))
writeLines(c("\\name{handwritten}", "\\alias{handwritten}", "\\title{Hand}"),
           file.path(pkg, "man", "handwritten.Rd"))
expect_false(tinyrox:::tinyrox_owns_rd(file.path(pkg, "man", "handwritten.Rd")))

# A second stale tinyrox-owned page (the copy carries the marker on line 1)
file.copy(file.path(pkg, "man", "foo.Rd"), file.path(pkg, "man", "old_topic.Rd"))

# Rename foo -> baz, so foo.Rd and old_topic.Rd become stale
writeLines(c("#' Baz", "#' @param x A number.", "#' @return x.", "#' @export",
             "baz <- function(x) x"),
           file.path(pkg, "R", "fns.R"))

# prune_rd = FALSE preserves the old behaviour: nothing removed
res_keep <- tinyrox::document(pkg, namespace = "none", cran_check = FALSE,
                              silent = TRUE, prune_rd = FALSE)
expect_equal(length(res_keep$pruned), 0L)
expect_true(file.exists(file.path(pkg, "man", "foo.Rd")))        # stale, kept
expect_true(file.exists(file.path(pkg, "man", "old_topic.Rd")))  # stale, kept
expect_true(file.exists(file.path(pkg, "man", "baz.Rd")))        # new

# prune_rd = TRUE removes only stale tinyrox-owned files
res <- tinyrox::document(pkg, namespace = "none", cran_check = FALSE,
                         silent = TRUE, prune_rd = TRUE)
expect_true(file.exists(file.path(pkg, "man", "baz.Rd")))         # current -> kept
expect_true(file.exists(file.path(pkg, "man", "handwritten.Rd"))) # no marker -> kept
expect_false(file.exists(file.path(pkg, "man", "foo.Rd")))        # stale tinyrox -> gone
expect_false(file.exists(file.path(pkg, "man", "old_topic.Rd")))  # stale tinyrox -> gone
expect_equal(sort(basename(res$pruned)), c("foo.Rd", "old_topic.Rd"))

unlink(pkg, recursive = TRUE)
