# Tests for install() and load_all()

# Create a temp package
tmp_pkg <- file.path(tempdir(), "testpkg")
if (dir.exists(tmp_pkg)) unlink(tmp_pkg, recursive = TRUE)
dir.create(tmp_pkg)
dir.create(file.path(tmp_pkg, "R"))

# Create minimal package
writeLines(c(
  "Package: testpkg",
  "Title: Test Package",
  "Version: 0.0.1",
  "Description: Test.",
  "License: MIT"
), file.path(tmp_pkg, "DESCRIPTION"))

writeLines("add <- function(x, y) x + y", file.path(tmp_pkg, "R", "add.R"))
writeLines("export(add)", file.path(tmp_pkg, "NAMESPACE"))

# Test quiet install
result <- rhydrogen::install(tmp_pkg, quiet = TRUE)
expect_true(result)

# Test load_all
files <- rhydrogen::load_all(tmp_pkg, quiet = TRUE)
expect_equal(length(files), 1)
expect_true(any(grepl("rhydrogen:testpkg", search())))

# Clean up search path
if ("rhydrogen:testpkg" %in% search()) {
  detach("rhydrogen:testpkg", character.only = TRUE)
}

# Clean up temp package
unlink(tmp_pkg, recursive = TRUE)
