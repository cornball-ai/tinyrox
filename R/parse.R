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

  # Group consecutive doc lines into blocks
  blocks <- list()
  current_block <- doc_lines[1]

  for (i in seq_along(doc_lines)[-1]) {
    if (doc_lines[i] == doc_lines[i - 1] + 1) {
      # Consecutive, add to current block
      current_block <- c(current_block, doc_lines[i])
    } else {
      # Gap found, save current block and start new one
      blocks <- c(blocks, list(current_block))
      current_block <- doc_lines[i]
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
#' @return Character vector of formal argument names.
#' @keywords internal
extract_formals <- function(code) {
  # Simple approach: extract content between first ( and matching )
  # This handles most cases but not multi-line signatures

  # Find the opening paren
  start <- regexpr("\\(", code)
  if (start == -1) return(character())

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
#' @return Character vector of argument names.
#' @keywords internal
parse_formals_text <- function(text) {
  if (nchar(trimws(text)) == 0) {
    return(character())
  }

  # Try to parse as a function and extract formals
  # This is more robust than regex
  fn_text <- paste0("function(", text, ") NULL")

  parsed <- tryCatch(
    parse(text = fn_text),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    # Fallback: simple split
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
    return(args[nchar(args) > 0])
  }

  # Extract formals from parsed function
  fn <- eval(parsed)
  names(formals(fn))
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
