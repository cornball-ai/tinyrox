# Tests for CRAN compliance checking

# Test get_dependency_packages
test_deps <- function() {
  desc <- matrix(
    c("mypkg", "A Package", "torch, av, jsonlite (>= 1.0)", "tinytest"),
    nrow = 1,
    dimnames = list(NULL, c("Package", "Title", "Imports", "Suggests"))
  )

  pkgs <- tinyrox:::get_dependency_packages(desc)
  expect_true("torch" %in% pkgs)
  expect_true("av" %in% pkgs)
  expect_true("jsonlite" %in% pkgs)
  expect_true("tinytest" %in% pkgs)
  expect_equal(length(pkgs), 4)
}
test_deps()

# Test find_unquoted_names
test_unquoted <- function() {
  # Unquoted torch should be found
  text1 <- "This uses torch for deep learning"
  expect_true("torch" %in% tinyrox:::find_unquoted_names(text1, "torch"))

  # Quoted 'torch' should NOT be found
  text2 <- "This uses 'torch' for deep learning"
  expect_equal(length(tinyrox:::find_unquoted_names(text2, "torch")), 0)

  # Mixed: one quoted, one not
  text3 <- "Uses 'torch' and also torch tensors"
  expect_true("torch" %in% tinyrox:::find_unquoted_names(text3, "torch"))

  # Multiple names
  text4 <- "Uses torch and OpenAI models"
  unquoted <- tinyrox:::find_unquoted_names(text4, c("torch", "OpenAI"))
  expect_true("torch" %in% unquoted)
  expect_true("OpenAI" %in% unquoted)
}
test_unquoted()

# Test quote_names_in_text
test_quoting <- function() {
  # Simple case
  text1 <- "Uses torch for inference"
  fixed1 <- tinyrox:::quote_names_in_text(text1, "torch")
  expect_equal(fixed1, "Uses 'torch' for inference")

  # Already quoted - should not double-quote

  text2 <- "Uses 'torch' for inference"
  fixed2 <- tinyrox:::quote_names_in_text(text2, "torch")
  expect_equal(fixed2, "Uses 'torch' for inference")

  # Multiple occurrences
  text3 <- "torch is great, torch is fast"
  fixed3 <- tinyrox:::quote_names_in_text(text3, "torch")
  expect_equal(fixed3, "'torch' is great, 'torch' is fast")

  # Mixed quoted and unquoted
  text4 <- "'torch' is great but torch needs quoting"
  fixed4 <- tinyrox:::quote_names_in_text(text4, "torch")
  expect_equal(fixed4, "'torch' is great but 'torch' needs quoting")
}
test_quoting()

# Test escape_regex
test_escape <- function() {
  # Dots should be escaped
  expect_equal(tinyrox:::escape_regex("data.table"), "data\\.table")
  # Normal text unchanged
  expect_equal(tinyrox:::escape_regex("torch"), "torch")
}
test_escape()

# Test check_webservice_links
test_weblinks <- function() {
  # hfhub without huggingface link
  desc1 <- "Downloads models from the hub"
  missing1 <- tinyrox:::check_webservice_links(desc1, "hfhub")
  expect_true("hfhub" %in% names(missing1))

  # hfhub with huggingface link - should not warn
  desc2 <- "Downloads from huggingface.co via hfhub"
  missing2 <- tinyrox:::check_webservice_links(desc2, "hfhub")
  expect_equal(length(missing2), 0)
}
test_weblinks()
