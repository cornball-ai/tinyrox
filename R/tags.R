#' Supported Documentation Tags
#'
#' @keywords internal
SUPPORTED_DOC_TAGS <- c(
  "title",
  "description",
  "details",
  "param",
  "return",
  "value",
  "examples",
  "example",
  "seealso",
  "references",
  "aliases",
  "keywords",
  "family",
  "name",
  "noRd",
  "inheritParams",
  "section",
  "author"
)

#' Supported Namespace Tags
#'
#' @keywords internal
SUPPORTED_NS_TAGS <- c(
  "export",
  "exportS3Method",
  "import",
  "importFrom",
  "useDynLib"
)

#' All Supported Tags
#'
#' @keywords internal
SUPPORTED_TAGS <- c(SUPPORTED_DOC_TAGS, SUPPORTED_NS_TAGS)

#' Parse Tags from Documentation Lines
#'
#' @param lines Character vector of documentation lines (without #').
#' @param object_name Name of the documented object.
#' @param file Source file (for error messages).
#' @param line_num Starting line number (for error messages).
#' @return A list with parsed tag values.
#' @keywords internal
parse_tags <- function(
  lines,
  object_name,
  file = NULL,
  line_num = NULL
) {
  result <- list(
    title = NULL,
    description = NULL,
    details = NULL,
    params = list(),
    return = NULL,
    examples = NULL,
    seealso = NULL,
    references = NULL,
    aliases = character(),
    keywords = character(),
    family = NULL,
    name = object_name,
    noRd = FALSE,
    export = FALSE,
    exportS3Method = NULL,
    imports = list(),
    importFroms = list(),
    useDynLib = NULL,
    inheritParams = character(),
    sections = list(),
    author = NULL
  )

  if (length(lines) == 0) {
    return(result)
  }

  # Track current tag and accumulator
  current_tag <- NULL
  current_arg <- NULL
  accumulator <- character()

  # Process lines
  for (i in seq_along(lines)) {
    line <- lines[i]

    # Check if line starts a new tag
    tag_match <- regexec("^@([a-zA-Z0-9]+)(\\s+(.*))?$", line)

    if (tag_match[[1]][1] != - 1) {
      # Save previous tag
      if (!is.null(current_tag)) {
        result <- save_tag(result, current_tag, current_arg, accumulator,
          file, line_num)
      }

      # Start new tag
      parts <- regmatches(line, tag_match) [[1]]
      current_tag <- parts[2]
      current_arg <- if (length(parts) >= 4 && nchar(parts[4]) > 0) {
        trimws(parts[4])
      } else {
        NULL
      }
      accumulator <- character()

      # Validate tag
      if (!current_tag %in% SUPPORTED_TAGS) {
        location <- if (!is.null(file)) {
          paste0(" at ", basename(file), ":", line_num + i - 1)
        } else {
          ""
        }
        stop("Unknown tag @", current_tag, location,
          "\nSupported tags: ", paste(SUPPORTED_TAGS, collapse = ", "),
          call. = FALSE)
      }
    } else if (!is.null(current_tag)) {
      # Continuation of current tag
      accumulator <- c(accumulator, line)
    } else {
      # Before any tag - this is title/description
      if (is.null(result$title) && nchar(trimws(line)) > 0) {
        result$title <- trimws(line)
      } else if (!is.null(result$title) && nchar(trimws(line)) > 0) {
        if (is.null(result$description)) {
          result$description <- trimws(line)
        } else {
          result$description <- paste(result$description, trimws(line))
        }
      }
    }
  }

  # Save final tag
  if (!is.null(current_tag)) {
    result <- save_tag(result, current_tag, current_arg, accumulator,
      file, line_num)
  }

  result
}

#' Save a Parsed Tag Value
#'
#' @keywords internal
save_tag <- function(
  result,
  tag,
  arg,
  accumulator,
  file,
  line_num
) {
  # Combine arg and accumulator
  value <- if (!is.null(arg) && length(accumulator) > 0) {
    paste(c(arg, accumulator), collapse = "\n")
  } else if (!is.null(arg)) {
    arg
  } else if (length(accumulator) > 0) {
    paste(accumulator, collapse = "\n")
  } else {
    ""
  }

  value <- trimws(value)

  switch(tag,
    "title" = {
      result$title <- value
    },
    "description" = {
      result$description <- value
    },
    "details" = {
      result$details <- value
    },
    "param" = {
      # Parse param: first word is name, rest is description
      parts <- strsplit(value, "\\s+", perl = TRUE) [[1]]
      if (length(parts) >= 1) {
        param_name <- parts[1]
        param_desc <- if (length(parts) > 1) {
          paste(parts[- 1], collapse = " ")
        } else {
          ""
        }
        result$params[[param_name]] <- param_desc
      }
    },
    "return" =,
    "value" = {
      result$return <- value
    },
    "examples" =,
    "example" = {
      # Examples are verbatim - include the arg if present
      if (!is.null(arg)) {
        result$examples <- paste(c(arg, accumulator), collapse = "\n")
      } else {
        result$examples <- paste(accumulator, collapse = "\n")
      }
    },
    "seealso" = {
      result$seealso <- value
    },
    "references" = {
      result$references <- value
    },
    "aliases" = {
      # Split on whitespace
      result$aliases <- c(result$aliases, strsplit(value, "\\s+") [[1]])
    },
    "keywords" = {
      result$keywords <- c(result$keywords, strsplit(value, "\\s+") [[1]])
    },
    "family" = {
      result$family <- value
    },
    "name" = {
      result$name <- value
    },
    "noRd" = {
      result$noRd <- TRUE
    },
    "export" = {
      result$export <- TRUE
    },
    "exportS3Method" = {
      # Parse: generic class
      parts <- strsplit(value, "\\s+") [[1]]
      if (length(parts) >= 2) {
        result$exportS3Method <- list(generic = parts[1], class = parts[2])
      } else if (length(parts) == 1 && nchar(parts[1]) > 0) {
        # Try to infer from object name (e.g., print.foo)
        result$exportS3Method <- list(explicit = parts[1])
      }
    },
    "import" = {
      result$imports <- c(result$imports, list(value))
    },
    "importFrom" = {
      # Parse: pkg sym1 sym2 ...
      parts <- strsplit(value, "\\s+") [[1]]
      if (length(parts) >= 2) {
        result$importFroms <- c(result$importFroms, list(list(
              pkg = parts[1],
              symbols = parts[- 1]
            )))
      }
    },
    "useDynLib" = {
      result$useDynLib <- value
    },
    "inheritParams" = {
      # Store the source function name for potential future use
      # Currently just parsed and stored, not processed
      result$inheritParams <- c(result$inheritParams, value)
    },
    "section" = {
      # @section Title: content
      # arg contains "Title:" and accumulator contains the content
      # Don't use 'value' here since it combines arg + accumulator
      if (!is.null(arg) && grepl(":$", arg)) {
        sec_title <- sub(":$", "", arg)
        sec_content <- if (length(accumulator) > 0) {
          paste(accumulator, collapse = "\n")
        } else {
          ""
        }
        result$sections <- c(result$sections, list(list(
              title = sec_title,
              content = sec_content
            )))
      }
    },
    "author" = {
      result$author <- value
    }
  )

  result
}

