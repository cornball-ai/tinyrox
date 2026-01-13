#' Compare rhydrogen and roxygen2 Documentation Output
#'
#' This script compares the documentation output from rhydrogen and roxygen2
#' to ensure compatibility and identify differences.
#'
#' Usage:
#'   source("tools/compare_rhydrogen_roxygen.R")
#'   compare_docs("~/sttapi")      # Single package
#'   compare_all_cornyverse()       # All cornyverse packages

#' Compare documentation output for a single package
#'
#' @param pkg_path Path to the package root.
#' @param verbose Print detailed output.
#' @return List with comparison results (invisibly).
compare_docs <- function(pkg_path, verbose = TRUE) {
  pkg_path <- normalizePath(pkg_path, mustWork = TRUE)
  pkg_name <- basename(pkg_path)

  if (verbose) message("Comparing docs for: ", pkg_name, "\n")

  # Check roxygen2 is available

  if (!requireNamespace("roxygen2", quietly = TRUE)) {
    stop("roxygen2 is required for comparison. Install with: install.packages('roxygen2')")
  }

  # Create temp directories for output
  rhydrogen_dir <- tempfile(pattern = "rhydrogen_")
  roxygen_dir <- tempfile(pattern = "roxygen_")
  dir.create(rhydrogen_dir)
  dir.create(roxygen_dir)

  # Copy package to both temp dirs
  file.copy(pkg_path, rhydrogen_dir, recursive = TRUE)
  file.copy(pkg_path, roxygen_dir, recursive = TRUE)

  rhydrogen_pkg <- file.path(rhydrogen_dir, pkg_name)
  roxygen_pkg <- file.path(roxygen_dir, pkg_name)

  # Clean existing docs
  clean_docs(rhydrogen_pkg)
  clean_docs(roxygen_pkg)

  # Generate with rhydrogen
  if (verbose) message("Generating with rhydrogen...")
  rhydrogen_result <- tryCatch({
    suppressWarnings(rhydrogen::document(rhydrogen_pkg, namespace = "overwrite"))
    list(success = TRUE, error = NULL)
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })

  # Generate with roxygen2
  if (verbose) message("Generating with roxygen2...")
  roxygen_result <- tryCatch({
    suppressWarnings(suppressMessages(
      roxygen2::roxygenise(roxygen_pkg, roclets = c("rd", "namespace"))
    ))
    list(success = TRUE, error = NULL)
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })

  # Check for generation errors
  if (!rhydrogen_result$success) {
    warning("rhydrogen failed: ", rhydrogen_result$error)
  }
  if (!roxygen_result$success) {
    warning("roxygen2 failed: ", roxygen_result$error)
  }

  # Compare NAMESPACE
  namespace_diff <- compare_namespace(rhydrogen_pkg, roxygen_pkg, verbose)

  # Compare Rd files
  rd_diff <- compare_rd_files(rhydrogen_pkg, roxygen_pkg, verbose)

  # Summary
  result <- list(
    package = pkg_name,
    rhydrogen_success = rhydrogen_result$success,
    roxygen_success = roxygen_result$success,
    namespace_identical = namespace_diff$identical,
    namespace_diff = namespace_diff,
    rd_files_identical = rd_diff$all_identical,
    rd_diff = rd_diff,
    rhydrogen_path = rhydrogen_pkg,
    roxygen_path = roxygen_pkg
  )

  if (verbose) {
    message("\n", strrep("=", 60))
    message("SUMMARY: ", pkg_name)
    message(strrep("=", 60))
    message("NAMESPACE: ", if (namespace_diff$identical) "IDENTICAL" else "DIFFERENT")
    message("Rd files:  ", if (rd_diff$all_identical) "IDENTICAL" else "DIFFERENT")
    message("  - Common files: ", length(rd_diff$common))
    message("  - rhydrogen only: ", length(rd_diff$rhydrogen_only))
    message("  - roxygen2 only: ", length(rd_diff$roxygen_only))
    if (!rd_diff$all_identical && length(rd_diff$differences) > 0) {
      message("  - Files with differences: ", length(rd_diff$differences))
    }
  }

  # Cleanup temp dirs (but keep them if there are differences for inspection)
  if (namespace_diff$identical && rd_diff$all_identical) {
    unlink(rhydrogen_dir, recursive = TRUE)
    unlink(roxygen_dir, recursive = TRUE)
    result$rhydrogen_path <- NULL
    result$roxygen_path <- NULL
  } else if (verbose) {
    message("\nTemp dirs retained for inspection:")
    message("  rhydrogen: ", rhydrogen_pkg)
    message("  roxygen2:  ", roxygen_pkg)
  }

  invisible(result)
}

#' Clean existing documentation files
#' @keywords internal
clean_docs <- function(pkg_path) {
  man_dir <- file.path(pkg_path, "man")
  if (dir.exists(man_dir)) {
    unlink(man_dir, recursive = TRUE)
  }
  dir.create(man_dir, showWarnings = FALSE)

  ns_file <- file.path(pkg_path, "NAMESPACE")
  if (file.exists(ns_file)) {
    unlink(ns_file)
  }
}

#' Compare NAMESPACE files
#' @keywords internal
compare_namespace <- function(rhydrogen_pkg, roxygen_pkg, verbose = FALSE) {
  rh_ns <- file.path(rhydrogen_pkg, "NAMESPACE")
  rx_ns <- file.path(roxygen_pkg, "NAMESPACE")

  rh_exists <- file.exists(rh_ns)
  rx_exists <- file.exists(rx_ns)

  if (!rh_exists && !rx_exists) {
    return(list(identical = TRUE, reason = "Both missing"))
  }

  if (rh_exists != rx_exists) {
    return(list(
      identical = FALSE,
      reason = paste("Only exists in:",
                     if (rh_exists) "rhydrogen" else "roxygen2")
    ))
  }

  # Read and normalize (remove comments, blank lines, sort)
  rh_lines <- normalize_namespace(readLines(rh_ns, warn = FALSE))
  rx_lines <- normalize_namespace(readLines(rx_ns, warn = FALSE))

  identical_content <- identical(rh_lines, rx_lines)

  if (!identical_content && verbose) {
    message("\nNAMESPACE differences:")

    # Show what's in rhydrogen but not roxygen
    rh_only <- setdiff(rh_lines, rx_lines)
    if (length(rh_only) > 0) {
      message("  rhydrogen only:")
      for (line in rh_only) message("    + ", line)
    }

    # Show what's in roxygen but not rhydrogen
    rx_only <- setdiff(rx_lines, rh_lines)
    if (length(rx_only) > 0) {
      message("  roxygen2 only:")
      for (line in rx_only) message("    + ", line)
    }
  }

  list(
    identical = identical_content,
    rhydrogen_lines = rh_lines,
    roxygen_lines = rx_lines,
    rhydrogen_only = setdiff(rh_lines, rx_lines),
    roxygen_only = setdiff(rx_lines, rh_lines)
  )
}

#' Normalize NAMESPACE content for comparison
#' @keywords internal
normalize_namespace <- function(lines) {
  # Remove comments
  lines <- grep("^#", lines, value = TRUE, invert = TRUE)
  # Remove blank lines
  lines <- lines[nzchar(trimws(lines))]
  # Sort for consistent comparison
  sort(lines)
}

#' Compare Rd files
#' @keywords internal
compare_rd_files <- function(rhydrogen_pkg, roxygen_pkg, verbose = FALSE) {
  rh_man <- file.path(rhydrogen_pkg, "man")
  rx_man <- file.path(roxygen_pkg, "man")

  rh_files <- list.files(rh_man, pattern = "\\.Rd$")
  rx_files <- list.files(rx_man, pattern = "\\.Rd$")

  common <- intersect(rh_files, rx_files)
  rh_only <- setdiff(rh_files, rx_files)
  rx_only <- setdiff(rx_files, rh_files)

  if (verbose && length(rh_only) > 0) {
    message("\nRd files only in rhydrogen: ", paste(rh_only, collapse = ", "))
  }
  if (verbose && length(rx_only) > 0) {
    message("Rd files only in roxygen2: ", paste(rx_only, collapse = ", "))
  }

  # Compare common files
  differences <- list()
  for (rd_file in common) {
    rh_content <- readLines(file.path(rh_man, rd_file), warn = FALSE)
    rx_content <- readLines(file.path(rx_man, rd_file), warn = FALSE)

    # Normalize for comparison
    rh_norm <- normalize_rd(rh_content)
    rx_norm <- normalize_rd(rx_content)

    if (!identical(rh_norm, rx_norm)) {
      diff_info <- compare_rd_content(rh_norm, rx_norm, rd_file)
      differences[[rd_file]] <- diff_info

      if (verbose) {
        message("\nDifferences in ", rd_file, ":")
        print_rd_diff(diff_info)
      }
    }
  }

  all_identical <- length(rh_only) == 0 &&
    length(rx_only) == 0 &&
    length(differences) == 0

  list(
    all_identical = all_identical,
    common = common,
    rhydrogen_only = rh_only,
    roxygen_only = rx_only,
    differences = differences
  )
}

#' Normalize Rd content for comparison
#' @keywords internal
normalize_rd <- function(lines) {
  # Remove auto-generated comments/headers
  lines <- grep("^% Generated by", lines, value = TRUE, invert = TRUE)
  lines <- grep("^% Please edit documentation", lines, value = TRUE, invert = TRUE)

  # Remove blank lines
  lines <- lines[nzchar(trimws(lines))]

  # Collapse and re-split to normalize whitespace within tags
  content <- paste(lines, collapse = "\n")

  # Normalize multiple spaces to single
  content <- gsub("[ \t]+", " ", content)

  # Split back
  strsplit(content, "\n")[[1]]
}

#' Compare normalized Rd content and identify differences
#' @keywords internal
compare_rd_content <- function(rh_lines, rx_lines, filename) {
  # Extract sections from each
  rh_sections <- extract_rd_sections(rh_lines)
  rx_sections <- extract_rd_sections(rx_lines)

  all_sections <- union(names(rh_sections), names(rx_sections))

  section_diffs <- list()
  for (sec in all_sections) {
    rh_sec <- rh_sections[[sec]]
    rx_sec <- rx_sections[[sec]]

    if (is.null(rh_sec) && !is.null(rx_sec)) {
      section_diffs[[sec]] <- list(type = "roxygen_only", content = rx_sec)
    } else if (!is.null(rh_sec) && is.null(rx_sec)) {
      section_diffs[[sec]] <- list(type = "rhydrogen_only", content = rh_sec)
    } else if (!identical(rh_sec, rx_sec)) {
      section_diffs[[sec]] <- list(
        type = "different",
        rhydrogen = rh_sec,
        roxygen = rx_sec
      )
    }
  }

  list(filename = filename, sections = section_diffs)
}

#' Extract sections from Rd content
#' @keywords internal
extract_rd_sections <- function(lines) {
  content <- paste(lines, collapse = "\n")

  # Match top-level Rd tags
  sections <- list()

  # Simple patterns for common sections
  patterns <- c(
    "name" = "\\\\name\\{([^}]+)\\}",
    "alias" = "\\\\alias\\{([^}]+)\\}",
    "title" = "\\\\title\\{([^}]+)\\}",
    "description" = "\\\\description\\{",
    "usage" = "\\\\usage\\{",
    "arguments" = "\\\\arguments\\{",
    "value" = "\\\\value\\{",
    "details" = "\\\\details\\{",
    "examples" = "\\\\examples\\{",
    "seealso" = "\\\\seealso\\{",
    "keyword" = "\\\\keyword\\{"
  )

  for (sec_name in names(patterns)) {
    matches <- gregexpr(patterns[[sec_name]], content, perl = TRUE)[[1]]
    if (matches[1] != -1) {
      sections[[sec_name]] <- TRUE
    }
  }

  sections
}

#' Print Rd diff in readable format
#' @keywords internal
print_rd_diff <- function(diff_info) {
  for (sec_name in names(diff_info$sections)) {
    sec <- diff_info$sections[[sec_name]]
    if (sec$type == "rhydrogen_only") {
      message("  \\", sec_name, ": rhydrogen only")
    } else if (sec$type == "roxygen_only") {
      message("  \\", sec_name, ": roxygen2 only")
    } else if (sec$type == "different") {
      message("  \\", sec_name, ": content differs")
    }
  }
}

#' Compare all cornyverse packages
#'
#' @param verbose Print detailed output.
#' @return Data frame summarizing results.
compare_all_cornyverse <- function(verbose = TRUE) {
  # Core cornyverse packages (in ~/)
  packages <- c(
    "cornyverse",
    "sttapi",
    "ttsapi",
    "xtxapi",
    "subtitles",
    "gpuctl",
    "diffuseR",
    "llamaR",
    "rife",
    "chatteRbox"
  )

  results <- list()
  for (pkg in packages) {
    pkg_path <- if (pkg == "cornyverse") {
      "~/cornyverse"
    } else {
      file.path("~", pkg)
    }

    pkg_path <- path.expand(pkg_path)

    if (!dir.exists(pkg_path)) {
      if (verbose) message("\nSkipping ", pkg, " (not found at ", pkg_path, ")")
      next
    }

    if (verbose) message("\n", strrep("=", 60), "\n")

    result <- tryCatch(
      compare_docs(pkg_path, verbose = verbose),
      error = function(e) {
        if (verbose) message("Error comparing ", pkg, ": ", e$message)
        list(
          package = pkg,
          rhydrogen_success = FALSE,
          roxygen_success = FALSE,
          namespace_identical = NA,
          rd_files_identical = NA,
          error = e$message
        )
      }
    )

    results[[pkg]] <- result
  }

  # Create summary data frame
  summary_df <- data.frame(
    package = vapply(results, function(x) x$package, character(1)),
    rhydrogen_ok = vapply(results, function(x) x$rhydrogen_success, logical(1)),
    roxygen_ok = vapply(results, function(x) x$roxygen_success, logical(1)),
    namespace_match = vapply(results, function(x) {
      if (is.na(x$namespace_identical)) NA else x$namespace_identical
    }, logical(1)),
    rd_match = vapply(results, function(x) {
      if (is.na(x$rd_files_identical)) NA else x$rd_files_identical
    }, logical(1)),
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message("\n", strrep("=", 60))
    message("OVERALL SUMMARY")
    message(strrep("=", 60))
    print(summary_df)
  }

  invisible(list(summary = summary_df, details = results))
}

#' Show detailed diff for a specific Rd file
#'
#' @param pkg_path Path to package.
#' @param rd_file Name of Rd file (e.g., "speech.Rd").
show_rd_diff <- function(pkg_path, rd_file) {
  pkg_path <- normalizePath(pkg_path, mustWork = TRUE)

  # Generate fresh comparison
  result <- compare_docs(pkg_path, verbose = FALSE)

  if (is.null(result$rhydrogen_path)) {
    message("No differences found - temp dirs were cleaned up")
    return(invisible(NULL))
  }

  rh_file <- file.path(result$rhydrogen_path, "man", rd_file)
  rx_file <- file.path(result$roxygen_path, "man", rd_file)

  if (!file.exists(rh_file) && !file.exists(rx_file)) {
    stop("File not found: ", rd_file)
  }

  message("=== rhydrogen ===")
  if (file.exists(rh_file)) {
    cat(readLines(rh_file, warn = FALSE), sep = "\n")
  } else {
    message("(not generated)")
  }

  message("\n=== roxygen2 ===")
  if (file.exists(rx_file)) {
    cat(readLines(rx_file, warn = FALSE), sep = "\n")
  } else {
    message("(not generated)")
  }

  invisible(list(rhydrogen = rh_file, roxygen = rx_file))
}

# Print usage when sourced
message("rhydrogen vs roxygen2 comparison tool loaded.")
message("Usage:")
message("  compare_docs('~/sttapi')     # Compare single package")
message("  compare_all_cornyverse()     # Compare all cornyverse packages")
message("  show_rd_diff('~/sttapi', 'speech.Rd')  # Show specific file diff")
