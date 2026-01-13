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
  doc_lines <- grep("^#'", lines)

  if (length(doc_lines) == 0) {
    return(list())
  }

  # Group doc lines into blocks, allowing gaps of comment/blank lines
  blocks <- list()
  current_block <- doc_lines[1]

  for (i in seq_along(doc_lines)[-1]) {
    prev_line <- doc_lines[i - 1]
    curr_line <- doc_lines[i]

    # Check if gap between prev and curr consists only of comments or blanks
    gap_ok <- TRUE
    if (curr_line > prev_line + 1) {
      gap_lines <- lines[(prev_line + 1):(curr_line - 1)]
      # Gap is OK if all intervening lines are comments or blank
      gap_ok <- all(grepl("^\\s*#|^\\s*$", gap_lines))
    }

    if (gap_ok) {
      # Continue current block
      current_block <- c(current_block, curr_line)
    } else {
      # Code found in gap - save current block and start new one
      blocks <- c(blocks, list(current_block))
      current_block <- curr_line
    }
  }
  blocks <- c(blocks, list(current_block))

  # Process each block
  result <- list()

  for (block_lines in blocks) {
    # Extract the comment text (strip #' prefix)
    comment_text <- sub("^#'\\s?", "", lines[block_lines])

    # Find the object definition after the block
    next_line <- max(block_lines) + 1

    # Skip blank lines
    while (next_line <= length(lines) && grepl("^\\s*$", lines[next_line])) {
      next_line <- next_line + 1
    }

    if (next_line > length(lines)) {
      next
    }

    # For multi-line function definitions, collect lines until we have complete signature
    # Look ahead up to 20 lines to capture full function signature
    definition_lines <- lines[next_line:min(next_line + 20, length(lines))]
    definition_text <- paste(definition_lines, collapse = "\n")

    # Parse the object definition
    obj_info <- parse_object_definition(definition_text, file, next_line)

    if (is.null(obj_info)) {
      # Check if it's a NULL documentation block (for namespace-only directives)
      first_def_line <- trimws(definition_lines[1])
      if (first_def_line == "NULL") {
        # Include block for namespace processing only
        result <- c(result, list(list(
          lines = comment_text,
          object = ".namespace_only",
          type = "namespace_only",
          formals = NULL,
          file = file,
          line = block_lines[1]
        )))
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
  # Match: name <- or name =
  # Handles: foo <- function(...), foo <- value, foo = function(...)

  # Get just the first line for name extraction
  first_line <- strsplit(text, "\n")[[1]][1]

  # Pattern for assignment
  pattern <- "^\\s*([a-zA-Z._][a-zA-Z0-9._]*)\\s*(<-|=)\\s*"
  match <- regexec(pattern, first_line)

  if (match[[1]][1] == -1) {
    return(NULL)
  }

  name <- regmatches(first_line, match)[[1]][2]

  # Check if it's a function (look in full text for multi-line defs)
  rest <- sub(pattern, "", text)

  if (grepl("^function\\s*\\(", rest)) {
    # It's a function - extract formals from potentially multi-line text
    formals_list <- extract_formals(rest)

    return(list(
      name = name,
      type = "function",
      formals = formals_list
    ))
  }

  # Not a function
  list(
    name = name,
    type = "other",
    formals = NULL
  )
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
  if (start == -1) return(list(names = character(), usage = character()))

  # Count parens to find the closing one
  chars <- strsplit(substr(code, start, nchar(code)), "")[[1]]
  depth <- 0
  end <- 0

  for (i in seq_along(chars)) {
    if (chars[i] == "(") depth <- depth + 1
    if (chars[i] == ")") depth <- depth - 1
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

  parsed <- tryCatch(
    parse(text = fn_text),
    error = function(e) NULL
  )

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
    if (missing(val) || identical(val, quote(expr = ))) {
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
