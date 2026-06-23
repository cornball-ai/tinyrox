# CRAN Compliance Checking
#
# Checks package code and examples for common CRAN issues. DESCRIPTION-field
# linting (software-name quoting, web-service links) is intentionally out of
# scope: tinyrox generates documentation, it is not a DESCRIPTION linter.

#' Full CRAN Compliance Check
#'
#' Runs the CRAN compliance checks for package code and examples.
#'
#' @param path Path to package root directory
#' @return List with all issues
#'
#' @export
#' @examples
#' \donttest{
#' # Create a minimal package in tempdir
#' pkg <- file.path(tempdir(), "mypkg")
#' dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
#' writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0\nDescription: A test package.",
#'     file.path(pkg, "DESCRIPTION"))
#' writeLines("add <- function(x, y) x + y",
#'     file.path(pkg, "R", "add.R"))
#'
#' check_cran(pkg)
#'
#' # Clean up
#' unlink(pkg, recursive = TRUE)
#' }
check_cran <- function(path = ".") {
    message("Checking CRAN compliance...")

    code_result <- check_code_cran(path)
    example_result <- check_examples_cran(path)

    all_issues <- list(code = code_result, examples = example_result)

    has_issues <- length(code_result) > 0 || length(example_result) > 0

    if (!has_issues) {
        message("No CRAN compliance issues found")
    }

    invisible(all_issues)
}

#' Check for Missing Examples
#'
#' Identifies exported functions that lack examples in their documentation.
#'
#' @param path Path to package root directory
#' @return Character vector of function names missing examples
#'
#' @export
#' @examples
#' \donttest{
#' # Create a minimal package in tempdir
#' pkg <- file.path(tempdir(), "mypkg")
#' dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
#' writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0",
#'     file.path(pkg, "DESCRIPTION"))
#' writeLines("#' Add numbers\n#' @export\nadd <- function(x, y) x + y",
#'     file.path(pkg, "R", "add.R"))
#'
#' check_examples_cran(pkg)
#'
#' # Clean up
#' unlink(pkg, recursive = TRUE)
#' }
check_examples_cran <- function(path = ".") {
    r_dir <- file.path(path, "R")
    if (!dir.exists(r_dir)) {
        stop("No R/ directory found in ", path, call. = FALSE)
    }

    r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE,
                          ignore.case = TRUE)

    if (length(r_files) == 0) {
        return(character())
    }

    missing_examples <- character()
    dontrun_fns <- character()
    long_lines <- character()

    for (file in r_files) {
        content <- readLines(file, warn = FALSE)
        file_missing <- find_exports_without_examples(content)
        missing_examples <- c(missing_examples, file_missing)
        file_dontrun <- find_dontrun_examples(content)
        dontrun_fns <- c(dontrun_fns, file_dontrun)
        file_long <- find_long_example_lines(content, basename(file))
        long_lines <- c(long_lines, file_long)
    }

    if (length(missing_examples) > 0) {
        warning("CRAN: Exported functions without examples: ",
                paste(missing_examples, collapse = ", "),
                call. = FALSE)
    }

    if (length(dontrun_fns) > 0) {
        warning("CRAN: Examples use \\dontrun (replace with \\donttest ",
                "unless truly non-executable): ",
                paste(dontrun_fns, collapse = ", "),
                call. = FALSE)
    }

    if (length(long_lines) > 0) {
        warning("CRAN: Example lines exceed 100 characters ",
                "(will be truncated in PDF manual):\n",
                paste("  ", long_lines, collapse = "\n"),
                call. = FALSE)
    }

    invisible(missing_examples)
}

#' Find Exported Functions Without Examples
#'
#' Parses R file content to find @export tags without @examples.
#'
#' @param lines Character vector of file lines
#' @return Character vector of function names missing examples
find_exports_without_examples <- function(lines) {
    missing <- character()
    in_doc_block <- FALSE
    has_export <- FALSE
    has_examples <- FALSE
    block_start <- 0

    for (i in seq_along(lines)) {
        line <- lines[i]

        # Start of doc block
        if (grepl("^#'", line)) {
            if (!in_doc_block) {
                in_doc_block <- TRUE
                has_export <- FALSE
                has_examples <- FALSE
                block_start <- i
            }

            # Check for export tag
            if (grepl("^#'\\s*@export", line)) {
                has_export <- TRUE
            }

            # Check for examples tag
            if (grepl("^#'\\s*@examples", line)) {
                has_examples <- TRUE
            }
        } else if (in_doc_block) {
            # End of doc block - check if it's a function definition
            in_doc_block <- FALSE

            if (has_export && !has_examples) {
                # Try to get function name from next non-blank line
                func_name <- extract_function_name(line)
                if (!is.null(func_name)) {
                    missing <- c(missing, func_name)
                }
            }
        }
    }

    missing
}

#' Find Exported Functions Using dontrun in Examples
#'
#' Parses R file content to find @export tags with \\dontrun in @examples.
#'
#' @param lines Character vector of file lines
#' @return Character vector of function names using dontrun
find_dontrun_examples <- function(lines) {
    found <- character()
    in_doc_block <- FALSE
    has_export <- FALSE
    has_dontrun <- FALSE

    for (i in seq_along(lines)) {
        line <- lines[i]

        if (grepl("^#'", line)) {
            if (!in_doc_block) {
                in_doc_block <- TRUE
                has_export <- FALSE
                has_dontrun <- FALSE
            }

            if (grepl("^#'\\s*@export", line)) {
                has_export <- TRUE
            }

            if (grepl("\\\\dontrun\\{", line)) {
                has_dontrun <- TRUE
            }
        } else if (in_doc_block) {
            in_doc_block <- FALSE

            if (has_export && has_dontrun) {
                func_name <- extract_function_name(line)
                if (!is.null(func_name)) {
                    found <- c(found, func_name)
                }
            }
        }
    }

    found
}

#' Find Example Lines Exceeding 100 Characters
#'
#' Scans @examples blocks for lines that will be truncated in the PDF manual.
#'
#' @param lines Character vector of file lines
#' @param filename Filename for reporting
#' @return Character vector of warnings (file:line format)
find_long_example_lines <- function(lines, filename) {
    found <- character()
    in_examples <- FALSE

    for (i in seq_along(lines)) {
        line <- lines[i]

        if (!grepl("^#'", line)) {
            in_examples <- FALSE
            next
        }

        if (grepl("^#'\\s*@examples", line)) {
            in_examples <- TRUE
            next
        }

        # Other tags end the examples block
        if (grepl("^#'\\s*@[a-zA-Z]", line)) {
            in_examples <- FALSE
            next
        }

        if (in_examples) {
            # Strip the #' prefix to get the actual example content
            content <- sub("^#'\\s?", "", line)
            if (nchar(content) > 100) {
                found <- c(found,
                           paste0(filename, ":", i, " (", nchar(content), " chars)"))
            }
        }
    }

    found
}

#' Extract Function Name from Definition Line
#'
#' @param line Code line potentially containing function definition
#' @return Function name or NULL
extract_function_name <- function(line) {
    # Match "name <- function" or "name = function"
    match <- regmatches(line,
                        regexec("^([A-Za-z_.][A-Za-z0-9_.]*)\\s*(<-|=)\\s*function", line))[[1]]
    if (length(match) >= 2) {
        return(match[2])
    }
    NULL
}
