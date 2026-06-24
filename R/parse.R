#' Parse R Files for Documentation Blocks
#'
#' Extracts documentation comment blocks and their associated objects
#' from R source files.
#'
#' @param file Path to an R source file.
#' @return A list of documentation blocks, each with components:
#'   - lines: character vector of comment lines (without #')
#'   - object: name of the documented object
#'   - type: "function", "data", or "other"
#'   - formals: for functions, the formal arguments
#' @keywords internal
parse_file <- function(file) {
    lines <- readLines(file, encoding = "UTF-8", warn = FALSE)

    # Find documentation blocks (consecutive #' lines)
    doc_lines <- grep("^##?'", lines)

    if (length(doc_lines) == 0) {
        return(list())
    }

    # Group doc lines into blocks: only strictly consecutive #' lines form
    # one block (roxygen2 semantics). Blocks separated by blank lines or
    # plain comments must stay separate - merging them lets an orphaned
    # block's tags (e.g. @export) bleed into the next function's block.
    blocks <- list()
    current_block <- doc_lines[1]

    for (i in seq_along(doc_lines)[-1]) {
        if (doc_lines[i] == doc_lines[i - 1] + 1) {
            # Continue current block
            current_block <- c(current_block, doc_lines[i])
        } else {
            # Any gap ends the block
            blocks <- c(blocks, list(current_block))
            current_block <- doc_lines[i]
        }
    }
    blocks <- c(blocks, list(current_block))

    # Process each block
    result <- list()

    for (block_lines in blocks) {
        # Extract the comment text (strip #' or ##' prefix)
        comment_text <- sub("^##?'\\s?", "", lines[block_lines])

        # Find the object definition after the block. Blank lines and plain
        # comments between the block and its object are allowed; another #'
        # line means this block is orphaned (its object was moved or deleted).
        next_line <- max(block_lines) + 1

        while (next_line <= length(lines) &&
            !grepl("^\\s*##?'", lines[next_line]) &&
            grepl("^\\s*(#|$)", lines[next_line])) {
            next_line <- next_line + 1
        }

        if (next_line > length(lines) || grepl("^\\s*##?'", lines[next_line])) {
            warning("Documentation block at ", basename(file), ":",
                    block_lines[1], " is not followed by an object ",
                    "definition; its tags are ignored.", call. = FALSE)
            next
        }

        # Collect the definition. For a multi-line function signature, read until
        # its parentheses balance rather than a fixed window (a fixed window
        # truncates long signatures and breaks formals parsing); a definition
        # with no opening paren is a one-liner and stops after its first line.
        end_line <- next_line
        depth <- 0L
        seen_paren <- FALSE
        repeat {
            chars <- strsplit(lines[end_line], "")[[1]]
            for (ch in chars) {
                if (ch == "(") {
                    depth <- depth + 1L
                    seen_paren <- TRUE
                } else if (ch == ")") {
                    depth <- depth - 1L
                }
            }
            if (!seen_paren || (depth <= 0L)) {
                break
            }
            if (end_line >= length(lines) || end_line - next_line >= 1000L) {
                break
            }
            end_line <- end_line + 1L
        }
        definition_lines <- lines[next_line:end_line]
        definition_text <- paste(definition_lines, collapse = "\n")

        # Parse the object definition
        obj_info <- parse_object_definition(definition_text, file, next_line)

        if (is.null(obj_info)) {
            # Check if it's a NULL documentation block (for namespace-only directives)
            first_def_line <- trimws(definition_lines[1])
            if (first_def_line == "NULL") {
                # Include block for namespace processing only
                result <- c(result, list(list(lines = comment_text,
                            object = ".namespace_only",
                            type = "namespace_only", formals = NULL,
                            file = file, line = block_lines[1])))
            }
            next
        }

        result <- c(result, list(list(
                                      lines = comment_text,
                                      object = obj_info$name,
                                      type = obj_info$type,
                                      formals = obj_info$formals,
                                      file = file,
                                      line = block_lines[1]
                )))
    }

    result
}

#' Parse Object Definition
#'
#' Identifies the object being defined from code text (may be multi-line).
#'
#' @param text The code text (may span multiple lines).
#' @param file The source file (for error messages).
#' @param line_num The line number (for error messages).
#' @return A list with name, type, and formals, or NULL if not a definition.
#' @keywords internal
parse_object_definition <- function(text, file, line_num) {
    # Get just the first line
    first_line <- strsplit(text, "\n")[[1]][1]

    # Skip indented definitions (likely methods inside nn_module/R6 classes)
    # Top-level definitions start at column 1 (no leading whitespace)
    # Methods inside class definitions are indented with 2+ spaces or tab
    if (grepl("^(\\s{2,}|\\t)", first_line)) {
        return(NULL)
    }

    # Check for "_PACKAGE" directive (package documentation)
    if (grepl('^"_PACKAGE"$', trimws(first_line))) {
        return(list(name = "_PACKAGE", type = "package", formals = NULL))
    }

    # Match: name <- or name =
    # Handles: foo <- function(...), foo <- value, foo = function(...)
    # Also handles backtick-quoted names: `%||%` <- function(...)
    # Pattern for regular names
    pattern <- "^\\s*([a-zA-Z._][a-zA-Z0-9._]*)\\s*(<-|=)\\s*"
    match <- regexec(pattern, first_line)

    # Pattern for backtick-quoted names (like `%||%`)
    backtick_pattern <- "^\\s*`([^`]+)`\\s*(<-|=)\\s*"
    backtick_match <- regexec(backtick_pattern, first_line)

    if (match[[1]][1] != -1) {
        name <- regmatches(first_line, match)[[1]][2]
        pattern_used <- pattern
    } else if (backtick_match[[1]][1] != -1) {
        name <- regmatches(first_line, backtick_match)[[1]][2]
        pattern_used <- backtick_pattern
    } else {
        return(NULL)
    }

    # Check if it's a function (look in full text for multi-line defs)
    rest <- sub(pattern_used, "", text)

    if (grepl("^function\\s*\\(", rest)) {
        # It's a function - extract formals from potentially multi-line text
        formals_list <- extract_formals(rest)

        return(list(name = name, type = "function", formals = formals_list))
    }

    # Check if it's a torch nn_module (extract formals from initialize method)
    if (grepl("^(torch::)?nn_module\\s*\\(", rest)) {
        formals_list <- extract_nn_module_formals(rest)

        return(list(name = name, type = "nn_module", formals = formals_list))
    }

    # Not a function - treat as data object
    list(name = name, type = "data", formals = NULL)
}

#' Extract Function Formals from Code
#'
#' @param code Code starting with "function("
#' @return List with 'names' (argument names) and 'usage' (formatted for Rd).
#' @keywords internal
extract_formals <- function(code) {
    # Simple approach: extract content between first ( and matching )
    # This handles most cases but not multi-line signatures

    # Find the opening paren
    start <- regexpr("\\(", code)
    if (start == -1) {
        return(list(names = character(), usage = character()))
    }

    # Count parens to find the closing one
    chars <- strsplit(substr(code, start, nchar(code)), "")[[1]]
    depth <- 0
    end <- 0

    for (i in seq_along(chars)) {
        if (chars[i] == "(") {
            depth <- depth + 1
        }
        if (chars[i] == ")") {
            depth <- depth - 1
        }
        if (depth == 0) {
            end <- i
            break
        }
    }

    if (end == 0) {
        # Didn't find closing paren - might be multi-line
        # For now, just extract what we have
        args_text <- substr(code, start + 1, nchar(code))
    } else {
        args_text <- substr(code, start + 1, start + end - 2)
    }

    # Parse the arguments
    # Split by comma, but be careful of defaults with commas
    parse_formals_text(args_text)
}

#' Extract Formals from torch nn_module
#'
#' Extracts the formals from the initialize method of an nn_module definition.
#'
#' @param code Code starting with "nn_module(" or "torch::nn_module("
#' @return List with 'names' (argument names) and 'usage' (formatted for Rd),
#'   or NULL if initialize method not found.
#' @keywords internal
extract_nn_module_formals <- function(code) {
    # Find initialize = function(...) pattern
    # Can be on same line or subsequent lines
    init_pattern <- "initialize\\s*=\\s*function\\s*\\("
    init_match <- regexpr(init_pattern, code)

    if (init_match == -1) {
        return(NULL)
    }

    # Extract from "function(" onwards
    init_start <- init_match + attr(init_match, "match.length") - 1
    rest <- substr(code, init_start, nchar(code))

    # Now extract formals like a regular function
    extract_formals(rest)
}

#' Parse Formals Text
#'
#' @param text Text containing function arguments.
#' @return List with 'names' (argument names) and 'usage' (formatted for Rd).
#' @keywords internal
parse_formals_text <- function(text) {
    if (nchar(trimws(text)) == 0) {
        return(list(names = character(), usage = character()))
    }

    # Try to parse as a function and extract formals
    # This is more robust than regex
    fn_text <- paste0("function(", text, ") NULL")

    parsed <- tryCatch(parse(text = fn_text), error = function(e) NULL)

    if (is.null(parsed)) {
        # Fallback: simple split - return just names, no usage
        parts <- strsplit(text, ",")[[1]]
        args <- vapply(parts, function(p) {
            # Extract name before = if present
            p <- trimws(p)
            if (grepl("=", p)) {
                trimws(sub("\\s*=.*", "", p))
            } else {
                p
            }
        }, character(1))
        args <- args[nchar(args) > 0]
        return(list(names = args, usage = args))
    }

    # Extract formals from parsed function
    fn <- eval(parsed)
    fmls <- formals(fn)
    arg_names <- names(fmls)

    # Build usage strings with defaults
    usage <- vapply(arg_names, function(nm) {
        val <- fmls[[nm]]
        if (missing(val) || identical(val, quote(expr =))) {
            # No default
            nm
        } else {
            # Has default - deparse it
            default <- deparse(val, width.cutoff = 500L)
            if (length(default) > 1) {
                default <- paste(default, collapse = " ")
            }
            paste0(nm, " = ", default)
        }
    }, character(1))

    list(names = arg_names, usage = unname(usage))
}

#' Parse All R Files in a Package
#'
#' @param path Path to package root.
#' @return List of all documentation blocks from all R files.
#' @keywords internal
parse_package <- function(path = ".") {
    r_dir <- file.path(path, "R")

    if (!dir.exists(r_dir)) {
        stop("No R/ directory found in ", path, call. = FALSE)
    }

    r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)

    all_blocks <- list()

    for (f in r_files) {
        blocks <- parse_file(f)
        all_blocks <- c(all_blocks, blocks)
    }

    all_blocks
}
