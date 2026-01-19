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

#' Check Package
#'
#' Runs R CMD build and R CMD check on the package.
#'
#' @param path Path to package root directory.
#' @param args Character vector of additional arguments to pass to R CMD check.
#'   Default includes "--as-cran" and "--no-manual".
#' @param error_on Severity level that causes an error: "error", "warning", or
#'   "note". Default is "warning" (fails on errors or warnings).
#'
#' @return TRUE if check passes, FALSE otherwise (invisibly).
#'   Also throws an error if check fails at or above error_on level.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check()
#' check(error_on = "error")  # Only fail on errors, not warnings
#' check(args = c("--as-cran", "--no-manual"))
#' }
check <- function(path = ".", args = c("--as-cran", "--no-manual"),
                  error_on = c("warning", "error", "note")) {
  error_on <- match.arg(error_on)

  # Get absolute path
  path <- normalizePath(path, mustWork = TRUE)

  # Get package name from DESCRIPTION
  desc_file <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    stop("No DESCRIPTION file found in ", path, call. = FALSE)
  }

  desc <- read.dcf(desc_file)
  pkg_name <- desc[1, "Package"]
  pkg_version <- desc[1, "Version"]

  # Create temp directory for build/check
  tmp_dir <- tempfile("rhydrogen_check_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Build the package
  message("Building ", pkg_name, "...")
  build_cmd <- paste("R CMD build", shQuote(path))
  old_wd <- setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  build_result <- system(build_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (build_result != 0) {
    # Re-run to show errors
    system(build_cmd)
    stop("R CMD build failed", call. = FALSE)
  }

  # Find the tarball
  tarball <- paste0(pkg_name, "_", pkg_version, ".tar.gz")
  if (!file.exists(tarball)) {
    stop("Expected tarball not found: ", tarball, call. = FALSE)
  }

  # Run R CMD check
  message("Checking ", pkg_name, "...")
  check_cmd <- paste("R CMD check", paste(args, collapse = " "), shQuote(tarball))
  check_result <- system(check_cmd)

  # Parse check results
  check_dir <- paste0(pkg_name, ".Rcheck")
  log_file <- file.path(check_dir, "00check.log")

  if (file.exists(log_file)) {
    log <- readLines(log_file, warn = FALSE)

    # Count issues (format: "* checking ... NOTE" or "ERROR: ...")
    errors <- sum(grepl("\\.\\.\\. ERROR$|^ERROR:", log))
    warnings <- sum(grepl("\\.\\.\\. WARNING$|^WARNING:", log))
    notes <- sum(grepl("\\.\\.\\. NOTE$|^NOTE:", log))

    # Print summary
    message("\n", pkg_name, " ", pkg_version, ": ",
            errors, " error(s), ", warnings, " warning(s), ", notes, " note(s)")

    # Determine if we should error
    should_error <- switch(error_on,
      "note" = errors > 0 || warnings > 0 || notes > 0,
      "warning" = errors > 0 || warnings > 0,
      "error" = errors > 0
    )

    if (should_error) {
      stop("R CMD check found issues", call. = FALSE)
    }

    invisible(TRUE)
  } else {
    if (check_result != 0) {
      stop("R CMD check failed", call. = FALSE)
    }
    invisible(TRUE)
  }
}

#' Install Package
#'
#' Wrapper around R CMD INSTALL with quiet mode option.
#'
#' @param path Path to package root directory.
#' @param quiet Logical. Suppress output except errors? Default TRUE.
#'
#' @return TRUE if successful, FALSE otherwise (invisibly).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install()
#' install(quiet = FALSE)  # Show full output
#' }
install <- function(path = ".", quiet = TRUE) {
  # Get absolute path
  path <- normalizePath(path, mustWork = TRUE)

  # Get package name from DESCRIPTION
  desc_file <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    stop("No DESCRIPTION file found in ", path, call. = FALSE)
  }

  desc <- read.dcf(desc_file)
  pkg_name <- desc[1, "Package"]

  # Build command
  cmd <- paste("R CMD INSTALL", shQuote(path))

  # Run install (redirect output if quiet)
  if (quiet) {
    # Redirect both stdout and stderr
    if (.Platform$OS.type == "windows") {
      cmd_quiet <- paste(cmd, "> NUL 2>&1")
    } else {
      cmd_quiet <- paste(cmd, "> /dev/null 2>&1")
    }
    result <- system(cmd_quiet)
  } else {
    result <- system(cmd)
  }

  if (result == 0) {
    message("Installed ", pkg_name)
    invisible(TRUE)
  } else {
    message("Install failed for ", pkg_name)
    invisible(FALSE)
  }
}

#' Load All Package Code
#'
#' Sources all R files in a package for interactive development,
#' without requiring a full install.
#'
#' @param path Path to package root directory.
#' @param quiet Logical. Suppress file sourcing messages? Default TRUE.
#'
#' @return Character vector of sourced files (invisibly).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_all()
#' load_all(quiet = FALSE)  # Show each file being sourced
#' }
load_all <- function(path = ".", quiet = TRUE) {
  r_dir <- file.path(path, "R")

  if (!dir.exists(r_dir)) {
    stop("No R/ directory found in ", path, call. = FALSE)
  }

  r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)

  if (length(r_files) == 0) {
    message("No R files found.")
    return(invisible(character()))
  }

  # Create a new environment attached to the search path
  pkg_env <- new.env(parent = globalenv())

  for (f in r_files) {
    if (!quiet) message("Sourcing ", basename(f))
    source(f, local = pkg_env)
  }

  # Attach the environment
  # Use a name that won't conflict
  env_name <- paste0("rhydrogen:", basename(normalizePath(path)))

  # Detach if already attached
  if (env_name %in% search()) {
    detach(env_name, character.only = TRUE)
  }

  attach(pkg_env, name = env_name)

  message("Loaded ", length(r_files), " file(s) as '", env_name, "'")
  invisible(r_files)
}

#' Reload an Installed Package
#'
#' Unloads a package if loaded, reinstalls it, and loads it again.
#' Convenience function for the document-install-reload cycle during development.
#'
#' @param path Path to package root directory.
#' @param document If TRUE (default), run document() before installing.
#' @param quiet Logical. Suppress install output? Default TRUE.
#'
#' @return TRUE if successful (invisibly).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' reload()  # Document, install, and reload current package
#' reload(document = FALSE)  # Just reinstall and reload
#' }
reload <- function(path = ".", document = TRUE, quiet = TRUE) {
  # Get package name from DESCRIPTION
  desc_file <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    stop("No DESCRIPTION file found in ", path, ". Is this an R package?", call. = FALSE)
  }

  desc <- read.dcf(desc_file)
  pkg_name <- desc[1, "Package"]

  # Document first if requested
  if (document) {
    document(path)
  }

  # Unload package if loaded
  pkg_loaded <- paste0("package:", pkg_name)
  if (pkg_loaded %in% search()) {
    tryCatch({
      detach(pkg_loaded, unload = TRUE, character.only = TRUE)
      message("Unloaded ", pkg_name)
    }, error = function(e) {
      # Sometimes unload fails due to dependencies, just detach
      tryCatch({
        detach(pkg_loaded, character.only = TRUE)
        message("Detached ", pkg_name, " (could not fully unload)")
      }, error = function(e2) {
        message("Note: Could not detach ", pkg_name, ": ", e2$message)
      })
    })
  }

  # Also unload namespace if still loaded
  if (pkg_name %in% loadedNamespaces()) {
    tryCatch({
      unloadNamespace(pkg_name)
    }, error = function(e) {
      # Ignore - will be handled by library() reload
    })
  }

  # Reinstall
  success <- install(path, quiet = quiet)

  if (success) {
    # Reload
    library(pkg_name, character.only = TRUE)
    message("Reloaded ", pkg_name)
  }

  invisible(success)
}
