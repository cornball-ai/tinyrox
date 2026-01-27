#' CRAN Release Utilities
#'
#' Functions for submitting packages to CRAN and testing on Windows.
#'
#' @importFrom curl curl_upload curl_fetch_memory new_handle handle_setform form_file
NULL

#' Build Package Tarball
#'
#' Builds a source package tarball suitable for CRAN submission.
#'
#' @param path Path to package root directory.
#' @param dest_dir Directory to place the tarball. Default is current directory.
#'
#' @return Path to the built tarball (invisibly).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' build()
#' build(dest_dir = tempdir())
#' }
build <- function(
  path = ".",
  dest_dir = "."
) {

  path <- normalizePath(path, mustWork = TRUE)
  dest_dir <- normalizePath(dest_dir, mustWork = TRUE)

  desc_file <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    stop("No DESCRIPTION file found in ", path, call. = FALSE)
  }

  desc <- read.dcf(desc_file)
  pkg_name <- desc[1, "Package"]
  pkg_version <- desc[1, "Version"]

  message("Building ", pkg_name, " ", pkg_version, "...")

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dest_dir)

  cmd <- paste("R CMD build", shQuote(path))
  result <- system(cmd)

  if (result != 0) {
    stop("R CMD build failed", call. = FALSE)
  }

  tarball <- file.path(dest_dir, paste0(pkg_name, "_", pkg_version, ".tar.gz"))
  if (!file.exists(tarball)) {
    stop("Expected tarball not found: ", tarball, call. = FALSE)
  }

  message("Built: ", tarball)
  invisible(tarball)
}

#' Get Package Maintainer
#'
#' Extracts pkg_maintainer name and email from DESCRIPTION.
#'
#' @param path Path to package root directory.
#'
#' @return A list with elements `name` and `email`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pkg_maintainer()
#' }
pkg_maintainer <- function(path = ".") {
  desc_file <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    stop("No DESCRIPTION file found in ", path, call. = FALSE)
  }

  desc <- read.dcf(desc_file)

  # Try Authors@R first
  if ("Authors@R" %in% colnames(desc)) {
    authors_r <- desc[1, "Authors@R"]
    # Parse the R expression
    authors <- tryCatch(
      eval(parse(text = authors_r)),
      error = function(e) NULL
    )

    if (!is.null(authors)) {
      # Find the pkg_maintainer (cre role)
      if (inherits(authors, "person")) {
        for (i in seq_along(authors)) {
          auth <- authors[i]
          if ("cre" %in% auth$role) {
            return(list(
                name = paste(auth$given, auth$family),
                email = auth$email
              ))
          }
        }
      }
    }
  }

  # Fall back to Maintainer field
  if ("Maintainer" %in% colnames(desc)) {
    maint <- desc[1, "Maintainer"]
    # Parse "Name <email>" format
    match <- regmatches(maint, regexec("^(.+?)\\s*<(.+)>$", maint)) [[1]]
    if (length(match) == 3) {
      return(list(
          name = trimws(match[2]),
          email = trimws(match[3])
        ))
    }
  }

  stop("Could not determine pkg_maintainer from DESCRIPTION", call. = FALSE)
}

#' Check Package on Windows via win-builder
#'
#' Uploads package to win-builder.r-project.org for testing on Windows.
#' Results are emailed to the package pkg_maintainer.
#'
#' @param path Path to package root directory.
#' @param r_version Which R version to test: "devel", "release", or "oldrelease".
#'   Default is "devel".
#'
#' @return TRUE if upload succeeded (invisibly).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_win_devel()
#' check_win_devel(r_version = "release")
#' }
check_win_devel <- function(
  path = ".",
  r_version = c("devel", "release", "oldrelease")
) {
  r_version <- match.arg(r_version)

  # Build the package
  tmp_dir <- tempfile("tinyrox_winbuild_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  tarball <- build(path, dest_dir = tmp_dir)

  # Get pkg_maintainer email for confirmation
  maint <- pkg_maintainer(path)
  message("Results will be emailed to: ", maint$email)

  # FTP URL based on R version
  ftp_dir <- switch(r_version,
    "devel" = "R-devel",
    "release" = "R-release",
    "oldrelease" = "R-oldrelease"
  )
  ftp_url <- paste0("ftp://win-builder.r-project.org/", ftp_dir, "/")

  message("Uploading to win-builder (", r_version, ")...")

  # Upload via FTP using curl
  result <- tryCatch({
      curl::curl_upload(tarball, ftp_url)
      TRUE
    }, error = function(e) {
      stop("FTP upload failed: ", e$message, call. = FALSE)
    })

  message("Upload complete. Check your email for results (usually within 30 minutes).")
  invisible(TRUE)
}

#' Submit Package to CRAN
#'
#' Uploads package to CRAN for review. You will receive a confirmation email
#' that must be clicked to complete the submission.
#'
#' @param path Path to package root directory.
#' @param comments Path to cran-comments.md file, or NULL to skip.
#'
#' @return TRUE if submission succeeded (invisibly).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' submit_cran()
#' }
submit_cran <- function(
  path = ".",
  comments = "cran-comments.md"
) {
  path <- normalizePath(path, mustWork = TRUE)

  # Get package info
  desc_file <- file.path(path, "DESCRIPTION")
  desc <- read.dcf(desc_file)
  pkg_name <- desc[1, "Package"]
  pkg_version <- desc[1, "Version"]

  # Get pkg_maintainer
  maint <- pkg_maintainer(path)

  message("Package: ", pkg_name, " ", pkg_version)
  message("Maintainer: ", maint$name, " <", maint$email, ">")

  # Confirm email
  response <- readline(paste0("Is your email address correct? (y/n): "))
  if (!tolower(response) %in% c("y", "yes")) {
    message("Submission cancelled. Update the pkg_maintainer email in DESCRIPTION.")
    return(invisible(FALSE))
  }

  # Read comments
  comment_text <- ""
  comments_path <- file.path(path, comments)
  if (!is.null(comments) && file.exists(comments_path)) {
    comment_text <- paste(readLines(comments_path, warn = FALSE), collapse = "\n")
    message("Including comments from: ", comments)
  }

  # Build the package
  tmp_dir <- tempfile("tinyrox_cran_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  tarball <- build(path, dest_dir = tmp_dir)
  tarball_size <- file.info(tarball) $size
  message("Package size: ", format(structure(tarball_size, class = "object_size"),
      units = "auto"))

  # Final confirmation
  response <- readline(paste0("Ready to submit ", pkg_name, " ", pkg_version,
      " to CRAN? (y/n): "))
  if (!tolower(response) %in% c("y", "yes")) {
    message("Submission cancelled.")
    return(invisible(FALSE))
  }

  # CRAN submission URL
  cran_url <- "https://xmpalantir.wu.ac.at/cransubmit/index2.php"

  message("Uploading to CRAN...")

  # Submit
  response <- tryCatch({
      h <- curl::new_handle()
      curl::handle_setform(h,
        name = maint$name,
        email = maint$email,
        uploaded_file = curl::form_file(tarball, type = "application/x-gzip"),
        comment = comment_text,
        upload = "Upload package"
      )
      curl::curl_fetch_memory(cran_url, handle = h)
    }, error = function(e) {
      stop("Upload failed: ", e$message, call. = FALSE)
    })

  # Check response
  if (response$status_code == 200) {
    # Parse response to get package ID for confirmation
    response_text <- rawToChar(response$content)

    # Look for the package ID in the response
    id_match <- regmatches(response_text,
      regexec('name="pkg_id"[^>]*value="([^"]+)"', response_text)) [[1]]

    if (length(id_match) >= 2) {
      pkg_id <- id_match[2]

      # Submit confirmation
      h2 <- curl::new_handle()
      curl::handle_setform(h2,
        pkg_id = pkg_id,
        name = maint$name,
        email = maint$email,
        policy_check = "1",
        submit = "Submit package"
      )
      confirm_response <- curl::curl_fetch_memory(cran_url, handle = h2)

      if (confirm_response$status_code == 200) {
        message("\nSubmission uploaded successfully.")
        message("Check your email (", maint$email, ") for a confirmation link.")
        message("You must click the link to complete the submission.")
        return(invisible(TRUE))
      }
    }

    # If we got here, something went wrong with confirmation
    message("\nPackage uploaded, but confirmation step may have failed.")
    message("Check your email for further instructions.")
    return(invisible(TRUE))

  } else if (response$status_code == 404) {
    # CRAN might be in maintenance mode
    response_text <- rawToChar(response$content)
    message("CRAN submission system returned 404.")
    message("The system may be in maintenance mode. Try again later.")
    return(invisible(FALSE))

  } else {
    stop("Submission failed with status: ", response$status_code, call. = FALSE)
  }
}

