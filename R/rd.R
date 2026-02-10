#' Generate Rd File Content
#'
#' @param tags Parsed tags from parse_tags().
#' @param formals Character vector of formal argument names (for functions).
#' @param source_file Source file path (for header comment).
#' @return Character string of Rd content.
#' @keywords internal
generate_rd <- function (tags, formals = NULL, source_file = NULL) {
    lines <- character()

    # Header comment - distinctively tinyrox
    lines <- c(lines, "% tinyrox says don't edit this manually, but it can't stop you!")

    # Required sections
    lines <- c(lines, paste0("\\name{", escape_rd(tags$name), "}"))
    lines <- c(lines, paste0("\\alias{", escape_rd(tags$name), "}"))

    # Additional aliases
    for (alias in tags$aliases) {
        if (alias != tags$name) {
            lines <- c(lines, paste0("\\alias{", escape_rd(alias), "}"))
        }
    }

    # Title (required)
    if (!is.null(tags$title)) {
        title <- tags$title
    } else {
        title <- tags$name
    }
    lines <- c(lines, paste0("\\title{", escape_rd(title), "}"))

    # Usage (for functions) - before arguments like roxygen2
    # Generate usage even for no-arg functions (formals is list with empty names)
    if (!is.null(formals)) {
        usage <- format_usage(tags$name, formals$usage)
        lines <- c(lines, "\\usage{")
        lines <- c(lines, escape_rd(usage))
        lines <- c(lines, "}")
    }

    # Arguments (for functions with params)
    if (length(tags$params) > 0) {
        lines <- c(lines, "\\arguments{")
        # Use formals order if available, otherwise param order
        if (!is.null(formals)) {
            formal_names <- formals$names
        } else {
            formal_names <- character()
        }
        param_order <- if (length(formal_names) > 0) {
            c(intersect(formal_names, names(tags$params)),
                setdiff(names(tags$params), formal_names))
        } else {
            names(tags$params)
        }
        for (i in seq_along(param_order)) {
            param <- param_order[i]
            desc_text <- escape_rd(tags$params[[param]])
            # Preserve param descriptions exactly as written (roxygen2 doesn't wrap)
            lines <- c(lines, paste0("\\item{", escape_rd(param), "}{", desc_text, "}"))
            # Add blank line between items (except after last)
            if (i < length(param_order)) {
                lines <- c(lines, "")
            }
        }
        lines <- c(lines, "}")
    }

    # Value/Return (before description like roxygen2)
    if (!is.null(tags$return)) {
        lines <- c(lines, "\\value{")
        lines <- c(lines, escape_rd(tags$return))
        lines <- c(lines, "}")
    }

    # Description
    desc <- if (!is.null(tags$description)) {
        tags$description
    } else if (!is.null(tags$title)) {
        tags$title
    } else {
        tags$name
    }
    lines <- c(lines, "\\description{")
    desc_escaped <- escape_rd(desc)
    # Preserve description exactly as written (roxygen2 doesn't wrap)
    lines <- c(lines, desc_escaped)
    lines <- c(lines, "}")

    # Details (after description like roxygen2)
    if (!is.null(tags$details)) {
        lines <- c(lines, "\\details{")
        lines <- c(lines, escape_rd(tags$details))
        lines <- c(lines, "}")
    }

    # References
    if (!is.null(tags$references)) {
        lines <- c(lines, "\\references{")
        lines <- c(lines, escape_rd(tags$references))
        lines <- c(lines, "}")
    }

    # Examples (before seealso like roxygen2)
    if (!is.null(tags$examples) && nchar(trimws(tags$examples)) > 0) {
        lines <- c(lines, "\\examples{")
        # Escape % in examples (Rd comment character), but leave other content verbatim
        examples_escaped <- gsub("%", "\\\\%", tags$examples)
        lines <- c(lines, examples_escaped)
        lines <- c(lines, "}")
    }

    # See Also (after examples like roxygen2)
    if (!is.null(tags$seealso)) {
        lines <- c(lines, "\\seealso{")
        lines <- c(lines, escape_rd(tags$seealso))
        lines <- c(lines, "}")
    }

    # Keywords
    for (kw in tags$keywords) {
        lines <- c(lines, paste0("\\keyword{", escape_rd(kw), "}"))
    }

    paste(lines, collapse = "\n")
}

#' Generate Rd File Content for Data Objects
#'
#' @param tags Parsed tags from parse_tags().
#' @param source_file Source file path (for header comment).
#' @param format_string Format description (e.g., "An object of class list of length 3").
#' @return Character string of Rd content.
#' @keywords internal
generate_data_rd <- function (tags, source_file = NULL, format_string = NULL) {
    lines <- character()

    # Header comment - distinctively tinyrox
    lines <- c(lines, "% tinyrox says don't edit this manually, but it can't stop you!")

    # docType for data
    lines <- c(lines, "\\docType{data}")

    # Required sections
    lines <- c(lines, paste0("\\name{", escape_rd(tags$name), "}"))
    lines <- c(lines, paste0("\\alias{", escape_rd(tags$name), "}"))

    # Additional aliases
    for (alias in tags$aliases) {
        if (alias != tags$name) {
            lines <- c(lines, paste0("\\alias{", escape_rd(alias), "}"))
        }
    }

    # Title (required)
    if (!is.null(tags$title)) {
        title <- tags$title
    } else {
        title <- tags$name
    }
    lines <- c(lines, paste0("\\title{", escape_rd(title), "}"))

    # Format section (roxygen2 auto-generates this)
    if (!is.null(format_string)) {
        lines <- c(lines, "\\format{")
        lines <- c(lines, format_string)
        lines <- c(lines, "}")
    }

    # Usage for data is just the object name
    lines <- c(lines, "\\usage{")
    lines <- c(lines, tags$name)
    lines <- c(lines, "}")

    # Description
    desc <- if (!is.null(tags$description)) {
        tags$description
    } else if (!is.null(tags$title)) {
        tags$title
    } else {
        tags$name
    }
    lines <- c(lines, "\\description{")
    desc_escaped <- escape_rd(desc)
    lines <- c(lines, desc_escaped)
    lines <- c(lines, "}")

    # Details
    if (!is.null(tags$details)) {
        lines <- c(lines, "\\details{")
        lines <- c(lines, escape_rd(tags$details))
        lines <- c(lines, "}")
    }

    # Examples (before seealso like roxygen2)
    if (!is.null(tags$examples) && nchar(trimws(tags$examples)) > 0) {
        lines <- c(lines, "\\examples{")
        examples_escaped <- gsub("%", "\\\\%", tags$examples)
        lines <- c(lines, examples_escaped)
        lines <- c(lines, "}")
    }

    # See Also
    if (!is.null(tags$seealso)) {
        lines <- c(lines, "\\seealso{")
        lines <- c(lines, escape_rd(tags$seealso))
        lines <- c(lines, "}")
    }

    # Keywords - add "datasets" for exported data objects (like roxygen2)
    # Don't add "datasets" if already marked as "internal"
    keywords <- tags$keywords
    if (!"datasets" %in% keywords && !"internal" %in% keywords) {
        keywords <- c(keywords, "datasets")
    }
    for (kw in keywords) {
        lines <- c(lines, paste0("\\keyword{", escape_rd(kw), "}"))
    }

    paste(lines, collapse = "\n")
}

#' Format Object Info for Data Documentation
#'
#' Generates the format description string for a data object.
#' Tries to load the package namespace to inspect the object.
#'
#' @param name Object name.
#' @param pkg_path Package root path.
#' @return Format string or NULL if object cannot be inspected.
#' @keywords internal
format_object_info <- function (name, pkg_path) {
    # Try to get object from package namespace
    pkg_name <- get_package_name(pkg_path)

    # First try: check if package is already loaded
    obj <- tryCatch(
        get(name, envir = asNamespace(pkg_name)),
        error = function (e) NULL
    )

    # Second try: source the R file and get the object
    if (is.null(obj)) {
        # Find the file that defines this object
        r_dir <- file.path(pkg_path, "R")
        r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)

        for (f in r_files) {
            lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
            pattern <- paste0("^\\s*", name, "\\s*(<-|=)")
            if (any(grepl(pattern, lines))) {
                # Try to source this file in a temp environment
                env <- new.env()
                tryCatch({
                        source(f, local = env, chdir = TRUE)
                        obj <- get(name, envir = env)
                    }, error = function(e) NULL)
                break
            }
        }
    }

    if (is.null(obj)) {
        return(NULL)
    }

    # Format like roxygen2: "An object of class \code{classname} of length N."
    obj_class <- class(obj)[1]
    obj_len <- length(obj)

    paste0("An object of class \\code{", obj_class, "} of length ", obj_len, ".")
}

#' Get Maintainer Info from DESCRIPTION
#'
#' Reads Authors@R from DESCRIPTION and formats maintainer for Rd.
#'
#' @param path Package root path.
#' @return Formatted author string or NULL.
#' @keywords internal
get_maintainer_from_desc <- function(path) {
    desc_file <- file.path(path, "DESCRIPTION")
    if (!file.exists(desc_file)) return(NULL)

    desc <- read.dcf(desc_file)
    if (!"Authors@R" %in% colnames(desc)) return(NULL)

    authors_r <- desc[1, "Authors@R"]
    # Parse the Authors@R field
    authors <- tryCatch(
        eval(parse(text = authors_r)),
        error = function(e) NULL
    )
    if (is.null(authors)) return(NULL)

    # Find maintainer (cre role)
    maintainers <- Filter(function(a) "cre" %in% a$role, authors)
    if (length(maintainers) == 0) return(NULL)

    # Format like roxygen2: \strong{Maintainer}: Name \email{email}
    m <- maintainers[[1]]
    name <- paste(m$given, m$family)
    email <- m$email
    if (!is.null(email) && nchar(email) > 0) {
        paste0("\\strong{Maintainer}: ", name, " \\email{", email, "}")
    } else {
        paste0("\\strong{Maintainer}: ", name)
    }
}

#' Escape Special Characters for Rd
#'
#' Escapes special characters for Rd format, but detects and preserves
#' existing Rd markup (like \\describe{}, \\item{}, \\code{}, etc.).
#'
#' @param text Text to escape.
#' @return Escaped text.
#' @keywords internal
escape_rd <- function(text) {
    if (is.null(text)) return("")

    # Check if text contains Rd markup (backslash commands like \describe, \item, etc.)
    # If so, pass through with minimal escaping (just %)
    if (grepl("\\\\[a-zA-Z]+\\{", text)) {
        # Contains Rd markup - only escape %
        text <- gsub("%", "\\\\%", text)
        return(text)
    }

    # No Rd markup - escape braces and percent only
    # Don't escape backslashes - unknown sequences pass through in Rd (like roxygen2)
    text <- gsub("\\{", "\\\\{", text)
    text <- gsub("\\}", "\\\\}", text)
    text <- gsub("%", "\\\\%", text)

    text
}

#' Format Usage Line
#'
#' Formats the function usage, wrapping to multiple lines if needed.
#' Follows roxygen2 style: each argument on its own line if total > 80 chars.
#' S3 methods use the \\method syntax.
#'
#' @param name Function name.
#' @param args Character vector of arguments with defaults.
#' @return Formatted usage string.
#' @keywords internal
format_usage <- function(
    name,
    args
) {
    # Check if it's an S3 method
    s3_info <- detect_s3_method(name)
    if (!is.null(s3_info)) {
        display_name <- paste0("\\method{", s3_info$generic, "}{", s3_info$class, "}")
    } else {
        display_name <- name
    }

    # Build single-line version
    single_line <- paste0(display_name, "(", paste(args, collapse = ", "), ")")

    # If short enough, use single line
    if (nchar(single_line) <= 80) {
        return(single_line)
    }

    # Otherwise, wrap to multiple lines like roxygen2
    # roxygen2 uses 2-space indent
    indent <- "  "
    lines <- character()
    lines <- c(lines, paste0(display_name, "("))

    for (i in seq_along(args)) {
        arg <- args[i]
        if (i < length(args)) {
            suffix <- ","
        } else {
            suffix <- ""
        }
        lines <- c(lines, paste0(indent, arg, suffix))
    }

    lines <- c(lines, ")")
    paste(lines, collapse = "\n")
}

#' Wrap Text to Width
#'
#' Wraps text to specified width, preserving words.
#'
#' @param text Text to wrap.
#' @param width Maximum line width.
#' @return Wrapped text with newlines.
#' @keywords internal
wrap_text <- function(
    text,
    width = 72
) {
    if (is.null(text) || nchar(text) <= width) {
        return(text)
    }

    # Split into words

    words <- strsplit(text, "\\s+") [[1]]
    if (length(words) == 0) return(text)

    lines <- character()
    current_line <- words[1]

    for (word in words[- 1]) {
        test_line <- paste(current_line, word)
        if (nchar(test_line) <= width) {
            current_line <- test_line
        } else {
            lines <- c(lines, current_line)
            current_line <- word
        }
    }
    lines <- c(lines, current_line)

    paste(lines, collapse = "\n")
}

#' Write Rd File
#'
#' @param content Rd content string.
#' @param name Topic name.
#' @param path Package root path.
#' @keywords internal
write_rd <- function(
    content,
    name,
    path = "."
) {
    man_dir <- file.path(path, "man")

    if (!dir.exists(man_dir)) {
        dir.create(man_dir, recursive = TRUE)
    }

    # Sanitize filename for Rd (like roxygen2)
    # - Internal functions starting with . -> dot-name
    # - Infix operators %X% -> grapes-X-grapes
    filename <- name
    if (grepl("^\\.", filename)) {
        filename <- gsub("^\\.", "dot-", filename)
    } else if (grepl("^%.*%$", filename)) {
        # Encode infix operator: %||% -> grapes-or-or-grapes
        # Only encode special characters, keep alphanumeric sequences intact
        inner <- gsub("^%|%$", "", filename)
        # Replace special characters with _WORD_ markers first
        inner <- gsub("\\|", "_OR_", inner)
        inner <- gsub("&", "_AND_", inner)
        inner <- gsub("\\+", "_PLUS_", inner)
        inner <- gsub("-", "_MINUS_", inner)
        inner <- gsub("\\*", "_TIMES_", inner)
        inner <- gsub("/", "_DIV_", inner)
        inner <- gsub("<", "_LT_", inner)
        inner <- gsub(">", "_GT_", inner)
        inner <- gsub("=", "_EQ_", inner)
        inner <- gsub("!", "_NOT_", inner)
        # Convert markers to lowercase with - separators
        inner <- gsub("_([A-Z]+)_", "-\\L\\1-", inner, perl = TRUE)
        # Clean up multiple - and leading/trailing -
        inner <- gsub("-+", "-", inner)
        inner <- gsub("^-|-$", "", inner)
        filename <- paste0("grapes-", inner, "-grapes")
    }
    filepath <- file.path(man_dir, paste0(filename, ".Rd"))

    writeLines(content, filepath, useBytes = TRUE)

    filepath
}

#' Generate Rd Content for Grouped Blocks (Multiple @rdname Entries)
#'
#' Merges multiple documentation blocks that share an @rdname topic
#' into a single .Rd file. The primary block (whose name matches the
#' topic) provides title/description; all blocks contribute usage/params.
#'
#' @param topic Topic name (the @rdname value).
#' @param entries List of list(tags, block) pairs sharing this topic.
#' @param all_tags All parsed tags (for @inheritParams resolution).
#' @return Character string of merged Rd content.
#' @keywords internal
generate_rd_grouped <- function(topic, entries, all_tags) {
    lines <- character()

    # Header
    lines <- c(lines, "% tinyrox says don't edit this manually, but it can't stop you!")

    # Find primary block: whose tags$name matches the topic name
    primary_idx <- NULL
    for (i in seq_along(entries)) {
        if (entries[[i]]$tags$name == topic) {
            primary_idx <- i
            break
        }
    }
    if (is.null(primary_idx)) primary_idx <- 1L
    primary <- entries[[primary_idx]]

    # \name{} - topic name
    lines <- c(lines, paste0("\\name{", escape_rd(topic), "}"))

    # \alias{} - all function names + explicit @aliases
    aliases_seen <- character()
    for (entry in entries) {
        nm <- entry$tags$name
        if (!nm %in% aliases_seen) {
            lines <- c(lines, paste0("\\alias{", escape_rd(nm), "}"))
            aliases_seen <- c(aliases_seen, nm)
        }
        for (alias in entry$tags$aliases) {
            if (!alias %in% aliases_seen) {
                lines <- c(lines, paste0("\\alias{", escape_rd(alias), "}"))
                aliases_seen <- c(aliases_seen, alias)
            }
        }
    }

    # \title{} - from primary
    title <- if (!is.null(primary$tags$title)) primary$tags$title else topic
    lines <- c(lines, paste0("\\title{", escape_rd(title), "}"))

    # \usage{} - one format_usage() per function block
    usage_lines <- character()
    for (entry in entries) {
        if (!is.null(entry$block$formals)) {
            usage_lines <- c(usage_lines,
                escape_rd(format_usage(entry$tags$name, entry$block$formals$usage)))
        }
    }
    if (length(usage_lines) > 0) {
        lines <- c(lines, "\\usage{")
        lines <- c(lines, paste(usage_lines, collapse = "\n\n"))
        lines <- c(lines, "}")
    }

    # \arguments{} - merged params (first definition wins), ordered by formals
    merged_params <- list()
    all_formal_names <- character()
    for (entry in entries) {
        # Resolve @inheritParams per entry
        etags <- entry$tags
        if (length(etags$inheritParams) > 0) {
            etags <- resolve_inherit_params(etags, all_tags, entry$block$formals)
        }
        if (!is.null(entry$block$formals)) {
            all_formal_names <- c(all_formal_names, entry$block$formals$names)
        }
        for (pname in names(etags$params)) {
            if (is.null(merged_params[[pname]])) {
                merged_params[[pname]] <- etags$params[[pname]]
            }
        }
    }
    if (length(merged_params) > 0) {
        # Order: formals order (unique, preserving first occurrence), then any extras
        all_formal_names <- unique(all_formal_names)
        param_order <- c(
            intersect(all_formal_names, names(merged_params)),
            setdiff(names(merged_params), all_formal_names)
        )
        lines <- c(lines, "\\arguments{")
        for (i in seq_along(param_order)) {
            param <- param_order[i]
            desc_text <- escape_rd(merged_params[[param]])
            lines <- c(lines, paste0("\\item{", escape_rd(param), "}{", desc_text, "}"))
            if (i < length(param_order)) lines <- c(lines, "")
        }
        lines <- c(lines, "}")
    }

    # \value{} - from primary, or first block that has one
    ret <- primary$tags$return
    if (is.null(ret)) {
        for (entry in entries) {
            if (!is.null(entry$tags$return)) {
                ret <- entry$tags$return
                break
            }
        }
    }
    if (!is.null(ret)) {
        lines <- c(lines, "\\value{")
        lines <- c(lines, escape_rd(ret))
        lines <- c(lines, "}")
    }

    # \description{} - from primary
    desc <- if (!is.null(primary$tags$description)) {
        primary$tags$description
    } else if (!is.null(primary$tags$title)) {
        primary$tags$title
    } else {
        topic
    }
    lines <- c(lines, "\\description{")
    lines <- c(lines, escape_rd(desc))
    lines <- c(lines, "}")

    # \details{} - concatenated from all blocks
    details_parts <- character()
    for (entry in entries) {
        if (!is.null(entry$tags$details)) {
            details_parts <- c(details_parts, entry$tags$details)
        }
    }
    if (length(details_parts) > 0) {
        lines <- c(lines, "\\details{")
        lines <- c(lines, escape_rd(paste(details_parts, collapse = "\n\n")))
        lines <- c(lines, "}")
    }

    # \references{} - from primary
    if (!is.null(primary$tags$references)) {
        lines <- c(lines, "\\references{")
        lines <- c(lines, escape_rd(primary$tags$references))
        lines <- c(lines, "}")
    }

    # \examples{} - concatenated from all blocks
    examples_parts <- character()
    for (entry in entries) {
        if (!is.null(entry$tags$examples) && nchar(trimws(entry$tags$examples)) > 0) {
            examples_parts <- c(examples_parts, entry$tags$examples)
        }
    }
    if (length(examples_parts) > 0) {
        lines <- c(lines, "\\examples{")
        lines <- c(lines, gsub("%", "\\\\%", paste(examples_parts, collapse = "\n\n")))
        lines <- c(lines, "}")
    }

    # \seealso{} - from primary
    if (!is.null(primary$tags$seealso)) {
        lines <- c(lines, "\\seealso{")
        lines <- c(lines, escape_rd(primary$tags$seealso))
        lines <- c(lines, "}")
    }

    # \keyword{} - union from all blocks
    all_kw <- character()
    for (entry in entries) {
        all_kw <- c(all_kw, entry$tags$keywords)
    }
    for (kw in unique(all_kw)) {
        lines <- c(lines, paste0("\\keyword{", escape_rd(kw), "}"))
    }

    paste(lines, collapse = "\n")
}

#' Generate All Rd Files for a Package
#'
#' @param blocks List of documentation blocks from parse_package().
#' @param path Package root path.
#' @return Character vector of generated file paths.
#' @keywords internal
generate_all_rd <- function(
    blocks,
    path = "."
) {
    generated <- character()

    # First pass: parse all blocks and build lookup for @inheritParams
    all_tags <- list()
    all_blocks <- list()

    for (block in blocks) {
        tags <- parse_tags(
            block$lines,
            block$object,
            block$file,
            block$line
        )

        # Skip namespace-only blocks UNLESS they have @name (documentation pages)
        # Pattern: #' Title\n#' @name foo\nNULL creates a standalone doc page
        if (block$type == "namespace_only" && tags$name == ".namespace_only") {
            next
        }

        all_tags[[tags$name]] <- tags
        all_blocks[[tags$name]] <- block
    }

    # Second pass: group blocks by topic (rdname or own name)
    # Each topic maps to a list of list(tags, block) entries
    topics <- list()
    topic_order <- character()  # preserve encounter order

    for (name in names(all_tags)) {
        tags <- all_tags[[name]]
        block <- all_blocks[[name]]

        # Skip if @noRd
        if (tags$noRd) next

        # Skip blocks with no documentation content (like roxygen2)
        # But keep blocks with @rdname - they merge into another block's page
        if (is.null(tags$title) && is.null(tags$description) &&
                is.null(tags$rdname)) next

        # Handle package documentation specially
        if (block$type == "package") {
            pkg_name <- get_package_name(path)
            rd_content <- generate_package_rd(tags, pkg_name, block$file)
            filepath <- write_rd(rd_content, paste0(pkg_name, "-package"), path)
            generated <- c(generated, filepath)
            next
        }

        # Determine topic: @rdname overrides, otherwise use block name
        topic <- if (!is.null(tags$rdname)) tags$rdname else tags$name

        if (is.null(topics[[topic]])) {
            topics[[topic]] <- list()
            topic_order <- c(topic_order, topic)
        }
        topics[[topic]] <- c(topics[[topic]], list(list(tags = tags, block = block)))
    }

    # Third pass: generate Rd for each topic
    for (topic in topic_order) {
        entries <- topics[[topic]]

        if (length(entries) == 1L) {
            # Single block - use existing generate_rd() path (unchanged behavior)
            tags <- entries[[1]]$tags
            block <- entries[[1]]$block

            # Resolve @inheritParams
            if (length(tags$inheritParams) > 0) {
                tags <- resolve_inherit_params(tags, all_tags, block$formals)
            }

            if (block$type == "data") {
                format_string <- format_object_info(tags$name, path)
                rd_content <- generate_data_rd(tags, block$file, format_string)
            } else {
                rd_content <- generate_rd(tags, block$formals, block$file)
            }

            filepath <- write_rd(rd_content, tags$name, path)
            generated <- c(generated, filepath)

            # Warn about undocumented params
            if (block$type %in% c("function", "nn_module") && !is.null(block$formals)) {
                formal_names <- block$formals$names
                undoc <- setdiff(formal_names, names(tags$params))
                undoc <- setdiff(undoc, "...")
                if (length(undoc) > 0) {
                    warning("Undocumented parameters in ", tags$name, ": ",
                        paste(undoc, collapse = ", "),
                        call. = FALSE)
                }
            }
        } else {
            # Multiple blocks sharing @rdname - generate merged Rd
            rd_content <- generate_rd_grouped(topic, entries, all_tags)
            filepath <- write_rd(rd_content, topic, path)
            generated <- c(generated, filepath)

            # Warn about undocumented params across all entries
            for (entry in entries) {
                if (entry$block$type %in% c("function", "nn_module") &&
                        !is.null(entry$block$formals)) {
                    formal_names <- entry$block$formals$names
                    undoc <- setdiff(formal_names, names(entry$tags$params))
                    undoc <- setdiff(undoc, "...")
                    if (length(undoc) > 0) {
                        warning("Undocumented parameters in ", entry$tags$name, ": ",
                            paste(undoc, collapse = ", "),
                            call. = FALSE)
                    }
                }
            }
        }
    }

    generated
}

#' Get Package Name from DESCRIPTION
#'
#' @param path Package root path.
#' @return Package name.
#' @keywords internal
get_package_name <- function(path) {
    desc_file <- file.path(path, "DESCRIPTION")
    if (!file.exists(desc_file)) {
        return("unknown")
    }
    desc <- read.dcf(desc_file, fields = "Package")
    as.character(desc[1, 1])
}

#' Generate Package Documentation Rd
#'
#' Generates Rd content for package documentation ("_PACKAGE" directive).
#'
#' @param tags Parsed tags from the documentation block.
#' @param pkg_name Package name.
#' @param source_file Source file path.
#' @return Character string of Rd content.
#' @keywords internal
generate_package_rd <- function(
    tags,
    pkg_name,
    source_file
) {
    lines <- character()

    # Header - distinctively tinyrox
    lines <- c(lines, "% tinyrox says don't edit this manually, but it can't stop you!")

    # docType
    lines <- c(lines, "\\docType{package}")

    # Name and aliases
    lines <- c(lines, paste0("\\name{", pkg_name, "-package}"))
    lines <- c(lines, paste0("\\alias{", pkg_name, "}"))
    lines <- c(lines, paste0("\\alias{", pkg_name, "-package}"))

    # Title - don't duplicate package name if title already starts with it
    title <- if (!is.null(tags$title)) {
        if (grepl(paste0("^", pkg_name, ":"), tags$title, ignore.case = TRUE)) {
            tags$title
        } else {
            paste0(pkg_name, ": ", tags$title)
        }
    } else {
        pkg_name
    }
    lines <- c(lines, paste0("\\title{", escape_rd(title), "}"))

    # Description
    if (!is.null(tags$description)) {
        lines <- c(lines, "\\description{")
        desc_escaped <- escape_rd(tags$description)
        # Preserve description exactly as written (roxygen2 doesn't wrap)
        lines <- c(lines, desc_escaped)
        lines <- c(lines, "}")
    }

    # Sections (like @section Core packages:)
    if (!is.null(tags$sections)) {
        for (sec in tags$sections) {
            lines <- c(lines, paste0("\\section{", escape_rd(sec$title), "}{"))
            lines <- c(lines, sec$content) # Content may have Rd markup, don't escape
            lines <- c(lines, "}")
        }
    }

    # Author (from tags or DESCRIPTION)
    if (!is.null(tags$author)) {
        lines <- c(lines, "\\author{")
        lines <- c(lines, escape_rd(tags$author))
        lines <- c(lines, "}")
    } else {
        # Try to get maintainer from DESCRIPTION (like roxygen2)
        # source_file is in R/, so go up one level to package root
        pkg_root <- dirname(dirname(source_file))
        author_info <- get_maintainer_from_desc(pkg_root)
        if (!is.null(author_info)) {
            lines <- c(lines, "\\author{")
            lines <- c(lines, author_info)
            lines <- c(lines, "}")
        }
    }

    # Keywords
    for (kw in tags$keywords) {
        lines <- c(lines, paste0("\\keyword{", escape_rd(kw), "}"))
    }

    paste(lines, collapse = "\n")
}

#' Resolve @inheritParams Tags
#'
#' Copies parameter documentation from source functions to the current function.
#' Only inherits params that are: (1) in the current function's formals, and
#' (2) not already documented in the current function.
#'
#' @param tags Current function's parsed tags.
#' @param all_tags Named list of all parsed tags (name -> tags).
#' @param formals Current function's formals (list with names and usage).
#' @return Updated tags with inherited params merged in.
#' @keywords internal
resolve_inherit_params <- function(
    tags,
    all_tags,
    formals
) {
    # Get the current function's formal parameter names

    if (!is.null(formals)) {
        formal_names <- formals$names
    } else {
        formal_names <- character()
    }

    for (source_name in tags$inheritParams) {
        # Handle pkg::function syntax
        if (grepl("::", source_name)) {
            # External package - skip for now (would need to load package)
            # TODO: Support external packages
            next
        }

        # Look up source function's tags
        source_tags <- all_tags[[source_name]]

        if (is.null(source_tags)) {
            warning("@inheritParams: source function '", source_name,
                "' not found in package", call. = FALSE)
            next
        }

        # Copy params that are in our formals and not already documented
        for (param_name in names(source_tags$params)) {
            # Only inherit if param is in our formals
            if (!param_name %in% formal_names) {
                next
            }

            # Only inherit if not already documented
            if (param_name %in% names(tags$params)) {
                next
            }

            # Inherit the param
            tags$params[[param_name]] <- source_tags$params[[param_name]]
        }
    }

    tags
}

