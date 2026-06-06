#' Generate Documentation for an R Package
#'
#' Main function for tinyrox. Parses R source files for documentation
#' comments and generates Rd files and NAMESPACE.
#'
#' @param path Path to package root directory. Default is current directory.
#' @param namespace How to handle NAMESPACE generation. One of
#' \describe{
#'   \item{"overwrite"}{Fully regenerate NAMESPACE (default)}
#'   \item{"append"}{Insert between ## tinyrox start/end markers}
#'   \item{"none"}{Don't modify NAMESPACE}
#' }
#' @param cran_check Run CRAN compliance checks (DESCRIPTION quoting,
#'   web service links, code issues, missing examples). Default TRUE.
#' @param silent Operate less verbose without messages. Default FALSE.
#' @return Invisibly returns a list with:
#'   - rd_files: character vector of generated Rd file paths
#'   - namespace: path to NAMESPACE file (or NULL if mode="none")
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create a minimal package in tempdir
#' pkg <- file.path(tempdir(), "mypkg")
#' dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
#' writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0",
#'     file.path(pkg, "DESCRIPTION"))
#' writeLines(c(
#'     "#' Add two numbers",
#'     "#' @param x A number",
#'     "#' @param y A number",
#'     "#' @export",
#'     "add <- function(x, y) x + y"),
#'     file.path(pkg, "R", "add.R"))
#'
#' # Document the package
#' document(pkg, cran_check = FALSE)
#'
#' # Clean up
#' unlink(pkg, recursive = TRUE)
#' }
document <- function(path = ".",
                     namespace = c("overwrite", "append", "none"),
                     cran_check = TRUE, silent = FALSE) {
    namespace <- match.arg(namespace)

    # Validate path
    if (!file.exists(file.path(path, "DESCRIPTION"))) {
        stop("No DESCRIPTION file found in ", path,
             ". Is this an R package?", call. = FALSE)
    }

    # Check CRAN compliance
    if (cran_check) {
        check_cran(path)
    }

    # Parse all R files
    if (!silent) message("Parsing R files...")
    blocks <- parse_package(path)

    if (length(blocks) == 0) {
        if (!silent) message("No documentation blocks found.")
        return(invisible(list(rd_files = character(), namespace = NULL)))
    }

    if (!silent) message("Found ", length(blocks), " documentation block(s).")

    # Generate Rd files
    if (!silent) message("Generating Rd files...")
    rd_files <- generate_all_rd(blocks, path, silent)
    if (!silent) message("Generated ", length(rd_files), " Rd file(s).")

    # Generate NAMESPACE
    ns_file <- NULL
    if (namespace != "none") {
        if (!silent) message("Generating NAMESPACE...")
        ns_content <- generate_namespace(blocks)
        ns_file <- write_namespace(ns_content, path, namespace)
        if (!silent) message("Updated NAMESPACE.")
    }

    if (!silent) message("Leaving DESCRIPTION alone as one should.")

    invisible(list(rd_files = rd_files, namespace = ns_file))
}

#' Clean Generated Files
#'
#' Removes all Rd files from man/ directory.
#'
#' @param path Path to package root directory.
#' @param namespace Also remove NAMESPACE? Default FALSE.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create a minimal package in tempdir
#' pkg <- file.path(tempdir(), "mypkg")
#' dir.create(file.path(pkg, "man"), recursive = TRUE, showWarnings = FALSE)
#' writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0",
#'     file.path(pkg, "DESCRIPTION"))
#' writeLines("placeholder", file.path(pkg, "man", "test.Rd"))
#'
#' clean(pkg)
#'
#' # Clean up
#' unlink(pkg, recursive = TRUE)
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

