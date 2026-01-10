#' Generate Documentation for an R Package
#'
#' Main function for rhydrogen. Parses R source files for documentation
#' comments and generates Rd files and NAMESPACE.
#'
#' @param path Path to package root directory. Default is current directory.
#' @param namespace How to handle NAMESPACE generation. One of:
#'   - "overwrite": Fully regenerate NAMESPACE (default)
#'   - "append": Insert between ## rhydrogen start/end markers
#'   - "none": Don't modify NAMESPACE
#'
#' @return Invisibly returns a list with:
#'   - rd_files: character vector of generated Rd file paths
#'   - namespace: path to NAMESPACE file (or NULL if mode="none")
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Document current package
#' document()
#'
#' # Document with append mode for NAMESPACE
#' document(namespace = "append")
#'
#' # Document without modifying NAMESPACE
#' document(namespace = "none")
#' }
document <- function(path = ".", namespace = c("overwrite", "append", "none")) {
  namespace <- match.arg(namespace)

  # Validate path
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    stop("No DESCRIPTION file found in ", path,
         ". Is this an R package?", call. = FALSE)
  }

  # Parse all R files
  message("Parsing R files...")
  blocks <- parse_package(path)

  if (length(blocks) == 0) {
    message("No documentation blocks found.")
    return(invisible(list(rd_files = character(), namespace = NULL)))
  }

  message("Found ", length(blocks), " documentation block(s).")

  # Generate Rd files
  message("Generating Rd files...")
  rd_files <- generate_all_rd(blocks, path)
  message("Generated ", length(rd_files), " Rd file(s).")

  # Generate NAMESPACE
  ns_file <- NULL
  if (namespace != "none") {
    message("Generating NAMESPACE...")
    ns_content <- generate_namespace(blocks)
    ns_file <- write_namespace(ns_content, path, namespace)
    message("Updated NAMESPACE.")
  }

  invisible(list(
    rd_files = rd_files,
    namespace = ns_file
  ))
}

#' Clean Generated Files
#'
#' Removes all Rd files from man/ directory.
#'
#' @param path Path to package root directory.
#' @param namespace Also remove NAMESPACE? Default FALSE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clean()
#' clean(namespace = TRUE)
#' }
clean <- function(path = ".", namespace = FALSE) {
  man_dir <- file.path(path, "man")

  if (dir.exists(man_dir)) {
    rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
    if (length(rd_files) > 0) {
      file.remove(rd_files)
      message("Removed ", length(rd_files), " Rd file(s).")
    }
  }

  if (namespace) {
    ns_file <- file.path(path, "NAMESPACE")
    if (file.exists(ns_file)) {
      file.remove(ns_file)
      message("Removed NAMESPACE.")
    }
  }

  invisible(NULL)
}

#' Check Package Documentation
#'
#' Validates that Rd files and NAMESPACE are syntactically correct.
#'
#' @param path Path to package root directory.
#'
#' @return TRUE if all checks pass, FALSE otherwise.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check()
#' }
check <- function(path = ".") {
  all_ok <- TRUE

  # Check Rd files
  man_dir <- file.path(path, "man")
  if (dir.exists(man_dir)) {
    rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)

    for (rd_file in rd_files) {
      result <- tryCatch({
        tools::parse_Rd(rd_file)
        TRUE
      }, error = function(e) {
        message("Error in ", basename(rd_file), ": ", e$message)
        FALSE
      })

      if (!result) all_ok <- FALSE
    }

    if (all_ok && length(rd_files) > 0) {
      message("All ", length(rd_files), " Rd files are valid.")
    }
  }

  # Check NAMESPACE
  ns_file <- file.path(path, "NAMESPACE")
  if (file.exists(ns_file)) {
    result <- tryCatch({
      parseNamespaceFile(basename(path), dirname(path))
      TRUE
    }, error = function(e) {
      # parseNamespaceFile is picky - try simpler check
      tryCatch({
        parse(file = ns_file)
        TRUE
      }, error = function(e2) {
        message("Error in NAMESPACE: ", e2$message)
        FALSE
      })
    })

    if (result) {
      message("NAMESPACE is valid.")
    } else {
      all_ok <- FALSE
    }
  }

  invisible(all_ok)
}
