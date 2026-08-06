# What a topic is called on disk.
#
# A topic can be called anything R can name, and `$.foo`, `[[.foo` and
# `$<-.foo` are ordinary S3 methods. Those characters are not legal in
# a file name everywhere R runs, and R CMD build does not warn and
# carry on -- it prints "excluding invalid files" and drops them from
# the tarball. The package installs with no docs for those methods,
# and R CMD check reports nothing, because it checks the tarball the
# files are no longer in.
#
# glinty shipped four undocumented S3 methods that way.

rd_filename <- tinyrox:::rd_filename
check_rd_filenames <- tinyrox:::check_rd_filenames

# --- the four that were being dropped ---
expect_equal(rd_filename("$.glinty_input"), "cash-.glinty_input")
expect_equal(rd_filename("[[.glinty_input"), "sub-sub-.glinty_input")
expect_equal(rd_filename("$<-.glinty_output"), "cash-set-.glinty_output")
expect_equal(rd_filename("[[<-.glinty_output"), "sub-subset-.glinty_output")

# --- what tinyrox already handled keeps its old name ---
#
# The table is roxygen2's, and these two come out of it unchanged, so
# adopting it renames nothing that was already working.
expect_equal(rd_filename("%||%"), "grapes-or-or-grapes")
expect_equal(rd_filename(".hidden"), "dot-hidden")
expect_equal(rd_filename(".onLoad"), "dot-onLoad")

# --- ordinary names are left alone ---
for (name in c("document", "write_rd", "run_app", "as.character.foo",
               "x2", "a.b.c", "with_underscores_and.dots")) {
    expect_equal(rd_filename(name), name, info = name)
}

# --- the rest of the operators ---
expect_equal(rd_filename("+.difftime"), "plus-.difftime")
expect_equal(rd_filename("[.data.frame"), "sub-.data.frame")
expect_equal(rd_filename("<-.foo"), "set-.foo")
expect_equal(rd_filename("=="), "equals")
expect_equal(rd_filename("!.foo"), "not-.foo")

# --- every result is portable, whatever went in ---
#
# The point is not the spelling, it is that nothing reaches disk that
# the build would throw away.
awkward <- c("$.a", "[[.b", "[<-.c", "%in%", "?.d", "@.e", "/.f", "\\.g",
             "{.h", "~.i", "|.j", "&.k", "*.l", "^.m", "#.n", "`.o",
             ";.p", "(.q", ").r", "<.s", ">.t", ":.u", "::v", "'.w",
             "\".x", "}.y", "].z")
for (name in awkward) {
    out <- rd_filename(name)
    expect_true(grepl("^[A-Za-z0-9._-]+$", out),
                info = paste(name, "->", out))
    # no leading dot, no doubled or dangling separators
    expect_false(startsWith(out, "."), info = out)
    expect_false(grepl("--", out, fixed = TRUE), info = out)
    expect_false(startsWith(out, "-") || endsWith(out, "-"), info = out)
}

# and distinct topics stay distinct, or one would overwrite the other
expect_equal(anyDuplicated(vapply(awkward, rd_filename, character(1))), 0L)

# --- the guard ---
expect_true(check_rd_filenames(c("man/ok.Rd", "man/also-ok.Rd")))
expect_error(check_rd_filenames("man/$.foo.Rd"), pattern = "not portable")
expect_error(check_rd_filenames(c("man/fine.Rd", "man/[[.bar.Rd")),
             pattern = "drop them from the tarball")
expect_true(check_rd_filenames(character(0)))

# --- end to end: a package with these methods builds with them ---
#
# Writing the file is the half that used to work. This checks the half
# that did not: that the name it lands under survives R CMD build.
pkg <- file.path(tempdir(), "rdnames")
dir.create(file.path(pkg, "man"), recursive = TRUE, showWarnings = FALSE)
written <- tinyrox:::write_rd("\\name{$.foo}\n\\title{x}\n", "$.foo", pkg)
expect_true(file.exists(written))
expect_equal(basename(written), "cash-.foo.Rd")
expect_true(grepl("^[A-Za-z0-9._-]+$", basename(written)))
unlink(pkg, recursive = TRUE)
