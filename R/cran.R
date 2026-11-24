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

    markdown_result <- check_markdown_rd(path)

    all_issues <- list(code = code_result, examples = example_result,
                       markdown = markdown_result)

    has_issues <- length(code_result) > 0 || length(example_result) > 0 ||
        length(markdown_result) > 0

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

    # document() parses the package again and reports parse warnings
    # (orphaned blocks, unknown tags) itself; repeating them here would
    # only print each one twice.
    blocks <- suppressWarnings(parse_package(path))
    missing_examples <- suppressWarnings(find_exports_without_examples(blocks))
    dontrun_fns <- character()
    long_lines <- character()

    for (file in r_files) {
        content <- readLines(file, warn = FALSE)
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
#' Examples belong to an Rd page, and blocks sharing an @rdname share one
#' page, so the check runs per topic: an exported function is reported only
#' when no block on its page carries examples. A block that generates no
#' page (a bare @export above an S3 method, or @noRd) is skipped, since
#' there is nowhere for an example to live.
#'
#' @param blocks List of documentation blocks from parse_package()
#' @return Character vector of function names missing examples
find_exports_without_examples <- function(blocks) {
    exported <- character()
    exported_topic <- character()
    topics_with_examples <- character()

    for (block in blocks) {
        if (!block$type %in% c("function", "nn_module", "data")) {
            next
        }
        tags <- parse_tags(block$lines, block$object, block$file, block$line)

        # Same page test as generate_all_rd()
        if (tags$noRd ||
            (is.null(tags$title) && is.null(tags$description) &&
                is.null(tags$rdname))) {
            next
        }

        if (!is.null(tags$rdname)) {
            topic <- tags$rdname
        } else {
            topic <- tags$name
        }

        if (!is.null(tags$examples) && nzchar(trimws(tags$examples))) {
            topics_with_examples <- c(topics_with_examples, topic)
        }

        is_exported <- tags$export || !is.null(tags$exportS3Method)
        if (is_exported && block$type == "function") {
            exported <- c(exported, block$object)
            exported_topic <- c(exported_topic, topic)
        }
    }

    exported[!exported_topic %in% topics_with_examples]
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

        if (grepl("^##?'", line)) {
            if (!in_doc_block) {
                in_doc_block <- TRUE
                has_export <- FALSE
                has_dontrun <- FALSE
            }

            if (grepl("^##?'\\s*@export", line)) {
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

        if (!grepl("^##?'", line)) {
            in_examples <- FALSE
            next
        }

        if (grepl("^##?'\\s*@examples", line)) {
            in_examples <- TRUE
            next
        }

        # Other tags end the examples block
        if (grepl("^##?'\\s*@[a-zA-Z]", line)) {
            in_examples <- FALSE
            next
        }

        if (in_examples) {
            # Strip the #' prefix to get the actual example content
            content <- sub("^##?'\\s?", "", line)
            if (nchar(content) > 100) {
                found <- c(found,
                           paste0(filename, ":", i, " (", nchar(content), " chars)"))
            }
        }
    }

    found
}

#' Warn About Markdown That Ships Verbatim
#'
#' tinyrox does no markdown parsing, so backticked text and bracketed
#' function links written in the roxygen2 markdown habit reach the Rd
#' unchanged and render literally in help pages and the PDF manual.
#' R CMD check says nothing about them. This reports each affected file
#' once. Set options(tinyrox.markdown_check = FALSE) to silence it for a
#' package that uses backticks on purpose.
#'
#' @param path Path to package root directory
#' @return Named list, one integer vector of line numbers per affected
#'   file (invisibly); empty when nothing was found or the check is off.
#' @keywords internal
check_markdown_rd <- function(path = ".") {
    if (!isTRUE(getOption("tinyrox.markdown_check", TRUE))) {
        return(invisible(list()))
    }

    r_files <- list.files(file.path(path, "R"), pattern = "\\.R$",
                          full.names = TRUE, ignore.case = TRUE)
    found <- list()
    for (file in r_files) {
        hits <- find_markdown_lines(readLines(file, warn = FALSE))
        if (length(hits) > 0) {
            found[[basename(file)]] <- hits
        }
    }

    if (length(found) > 0) {
        report <- vapply(names(found), function(f) {
            paste0("  ", f, ": ", length(found[[f]]), " line(s), first at ",
                   f, ":", found[[f]][1])
        }, character(1))
        warning("Markdown is not converted and ships verbatim in the Rd. ",
                "Write \\code{x} for a backticked x and \\code{\\link{fn}} ",
                "for a bracketed fn() link:\n",
                paste(report, collapse = "\n"),
                "\nSilence with options(tinyrox.markdown_check = FALSE).",
                call. = FALSE)
    }

    invisible(found)
}

#' Find Documentation Lines Containing Markdown
#'
#' Flags backtick pairs and bracketed function links in documentation
#' prose. Example code is skipped (backticks are R syntax there), as are
#' blocks tagged noRd (they ship nowhere) and text already inside an Rd
#' code macro.
#'
#' @param lines Character vector of file lines
#' @return Integer vector of line numbers
#' @keywords internal
find_markdown_lines <- function(lines) {
    is_doc <- grepl("^##?'", lines)
    if (!any(is_doc)) {
        return(integer())
    }

    # Consecutive doc lines form one block, as in parse_file()
    block_id <- cumsum(c(TRUE, diff(is_doc) != 0))
    text <- sub("^##?'\\s?", "", lines)
    found <- integer()

    for (id in unique(block_id[is_doc])) {
        idx <- which(block_id == id)
        if (any(grepl("^\\s*@noRd\\b", text[idx]))) {
            next
        }
        in_examples <- FALSE
        for (i in idx) {
            tag <- regmatches(text[i], regexec("^\\s*@([a-zA-Z0-9]+)", text[i]))[[1]]
            if (length(tag) == 2) {
                in_examples <- tag[2] %in% c("examples", "example",
                                             "examplesIf")
                if (tag[2] %in% c("name", "rdname", "aliases",
                                  SUPPORTED_NS_TAGS)) {
                    next
                }
            }
            if (in_examples) {
                next
            }
            # Drop spans already in an Rd code macro: \code{`[`} is correct
            prose <- gsub("\\\\(code|verb|samp|preformatted|eqn|deqn)\\{[^{}]*\\}",
                          "", text[i])
            if (grepl("`[^`]+`", prose) ||
                grepl("\\[[A-Za-z0-9._:]+\\(\\)\\]", prose)) {
                found <- c(found, i)
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
