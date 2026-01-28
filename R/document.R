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
#' @param cran_check Check DESCRIPTION for CRAN compliance issues
#'   (unquoted package names, missing web service links). Default TRUE.
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
#'
#' # Skip CRAN compliance check
#' document(cran_check = FALSE)
#' }
document <- function (path = ".", namespace = c("overwrite", "append", "none"),
                      cran_check = TRUE) {
    namespace <- match.arg(namespace)

    # Validate path
    if (!file.exists(file.path(path, "DESCRIPTION"))) {
        stop("No DESCRIPTION file found in ", path,
            ". Is this an R package?", call. = FALSE)
    }

    # Check CRAN compliance
    if (cran_check) {
        check_description_cran(path)
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
clean <- function (path = ".", namespace = FALSE) {
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

