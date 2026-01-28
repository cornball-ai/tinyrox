#' CRAN Compliance Checking for DESCRIPTION
#'
#' Functions to validate and fix common CRAN submission issues
#' related to the DESCRIPTION file.

# Known web services and their URLs
# Packages that typically require web service documentation
WEBSERVICE_PACKAGES <- list(

    hfhub = "https://huggingface.co/",
    huggingface = "https://huggingface.co/",
    openai = "https://platform.openai.com/",
    gh = "https://github.com/",
    googledrive = "https://drive.google.com/",
    googlesheets4 = "https://docs.google.com/spreadsheets/",
    aws.s3 = "https://aws.amazon.com/s3/",
    bigrquery = "https://cloud.google.com/bigquery/"
)

# Common software/API names that should be quoted (case-insensitive matching)
# These are checked in addition to package names from Imports/Suggests
KNOWN_SOFTWARE_NAMES <- c(
    "OpenAI", "Whisper", "GPT", "ChatGPT",
    "TensorFlow", "PyTorch", "Keras", "CUDA",
    "HuggingFace", "Hugging Face",
    "GitHub", "GitLab", "Bitbucket",
    "Docker", "Kubernetes",
    "JAGS", "Stan", "BUGS", "WinBUGS", "OpenBUGS",
    "Rcpp", "RcppArmadillo", "RcppEigen",
    "Shiny", "Plumber",
    "JSON", "XML", "YAML", "CSV", "Excel",
    "SQLite", "PostgreSQL", "MySQL", "MongoDB",
    "AWS", "Azure", "GCP"
)

#' Check DESCRIPTION for CRAN Compliance
#'
#' Validates Title and Description fields for common CRAN issues:
#' unquoted package names, missing web service links, etc.
#'
#' @param path Path to package root directory
#' @param fix If TRUE, return fixed text. If FALSE, just warn.
#' @return List with validation results and optionally fixed text
#'
#' @export
check_description_cran <- function (path = ".", fix = FALSE) {
    desc_file <- file.path(path, "DESCRIPTION")
    if (!file.exists(desc_file)) {
        stop("No DESCRIPTION file found in ", path, call. = FALSE)
    }

    desc <- read.dcf(desc_file)

    # Get fields

    title <- if ("Title" %in% colnames(desc)) desc[1, "Title"] else ""
    description <- if ("Description" %in% colnames(desc)) desc[1, "Description"] else ""

    # Get package dependencies
    dep_packages <- get_dependency_packages(desc)

    # Build list of names to check (packages + known software)
    check_names <- unique(c(dep_packages, KNOWN_SOFTWARE_NAMES))

    issues <- list()

    # Check for unquoted package/software names
    title_unquoted <- find_unquoted_names(title, check_names)
    desc_unquoted <- find_unquoted_names(description, check_names)

    if (length(title_unquoted) > 0) {
        issues$title_unquoted <- title_unquoted
        warning("CRAN: Unquoted names in Title: ",
            paste(title_unquoted, collapse = ", "),
            "\n  Use single quotes: ",
            paste0("'", title_unquoted, "'", collapse = ", "),
            call. = FALSE)
    }

    if (length(desc_unquoted) > 0) {
        issues$desc_unquoted <- desc_unquoted
        warning("CRAN: Unquoted names in Description: ",
            paste(desc_unquoted, collapse = ", "),
            "\n  Use single quotes: ",
            paste0("'", desc_unquoted, "'", collapse = ", "),
            call. = FALSE)
    }

    # Check for missing web service links
    missing_links <- check_webservice_links(description, dep_packages)
    if (length(missing_links) > 0) {
        issues$missing_links <- missing_links
        link_suggestions <- vapply(names(missing_links), function (pkg) {
                paste0(pkg, " <", missing_links[[pkg]], ">")
            }, character(1))
        warning("CRAN: Missing web service links in Description for: ",
            paste(names(missing_links), collapse = ", "),
            "\n  Consider adding: ", paste(link_suggestions, collapse = ", "),
            call. = FALSE)
    }

    result <- list(
        issues = issues,
        has_issues = length(issues) > 0
    )

    if (fix) {
        result$fixed_title <- quote_names_in_text(title, check_names)
        result$fixed_description <- quote_names_in_text(description, check_names)
    }

    invisible(result)
}

#' Extract Package Names from Dependencies
#'
#' Parses Imports, Suggests, Depends, and Enhances fields.
#'
#' @param desc DESCRIPTION matrix from read.dcf()
#' @return Character vector of package names
get_dependency_packages <- function (desc) {
    dep_fields <- c("Imports", "Suggests", "Depends", "Enhances", "LinkingTo")
    packages <- character()

    for (field in dep_fields) {
        if (field %in% colnames(desc) && !is.na(desc[1, field])) {
            field_val <- desc[1, field]
            # Split on comma, handle newlines
            pkgs <- strsplit(field_val, ",") [[1]]
            # Remove version specs like (>= 1.0.0) and trim whitespace
            pkgs <- gsub("\\s*\\([^)]*\\)", "", pkgs)
            pkgs <- trimws(pkgs)
            # Remove empty strings and R itself
            pkgs <- pkgs[pkgs != "" & pkgs != "R"]
            packages <- c(packages, pkgs)
        }
    }

    unique(packages)
}

#' Find Unquoted Names in Text
#'
#' Finds package/software names that appear without single quotes.
#'
#' @param text Text to search
#' @param names Names to look for
#' @return Character vector of unquoted names found
find_unquoted_names <- function (text, names) {
    if (is.na(text) || text == "") return(character())

    unquoted <- character()

    for (name in names) {
        # Skip very short names to avoid false positives
        if (nchar(name) < 2) next

        # Pattern: name NOT preceded by ' and NOT followed by '
        # Use word boundaries to avoid partial matches
        # But be careful with names containing special regex chars

        escaped_name <- escape_regex(name)

        # Check if name appears unquoted
        # Unquoted = appears but not as 'name'
        pattern_quoted <- paste0("'", escaped_name, "'")
        pattern_unquoted <- paste0("\\b", escaped_name, "\\b")

        # Find all occurrences
        if (grepl(pattern_unquoted, text, ignore.case = FALSE)) {
            # Check if ALL occurrences are quoted
            # Remove quoted versions and see if any remain
            text_without_quoted <- gsub(pattern_quoted, "", text)
            if (grepl(pattern_unquoted, text_without_quoted, ignore.case = FALSE)) {
                unquoted <- c(unquoted, name)
            }
        }
    }

    unique(unquoted)
}

#' Quote Names in Text
#'
#' Wraps unquoted package/software names in single quotes.
#'
#' @param text Text to modify
#' @param names Names to quote
#' @return Modified text with names quoted
quote_names_in_text <- function (text, names) {
    if (is.na(text) || text == "") return(text)

    for (name in names) {
        if (nchar(name) < 2) next

        escaped_name <- escape_regex(name)
        pattern_quoted <- paste0("'", escaped_name, "'")

        # Only replace unquoted instances
        # First, temporarily replace quoted instances
        placeholder <- paste0("\001QUOTED_", which(names == name), "\001")
        text <- gsub(pattern_quoted, placeholder, text)

        # Now quote unquoted instances (word boundary match)
        pattern_unquoted <- paste0("\\b(", escaped_name, ")\\b")
        text <- gsub(pattern_unquoted, "'\\1'", text)

        # Restore originally quoted instances
        text <- gsub(placeholder, pattern_quoted, text, fixed = TRUE)
    }

    text
}

#' Check for Missing Web Service Links
#'
#' Checks if packages that typically use web services have
#' corresponding URLs in the Description.
#'
#' @param description Description text
#' @param packages Package names from dependencies
#' @return Named list of packages missing links (name = URL)
check_webservice_links <- function (description, packages) {
    missing <- list()

    for (pkg in packages) {
        pkg_lower <- tolower(pkg)
        if (pkg_lower %in% names(WEBSERVICE_PACKAGES)) {
            expected_url <- WEBSERVICE_PACKAGES[[pkg_lower]]
            # Check if URL (or domain) appears in description
            domain <- gsub("https?://([^/]+).*", "\\1", expected_url)
            if (!grepl(domain, description, ignore.case = TRUE)) {
                missing[[pkg]] <- expected_url
            }
        }
    }

    missing
}

#' Escape Regex Special Characters
#'
#' @param x String to escape
#' @return Escaped string safe for use in regex
escape_regex <- function (x) {
    # Escape special regex metacharacters
    # Order matters: escape backslash first
    chars <- c("\\", ".", "|", "(", ")", "[", "]", "{", "}", "^", "$", "+", "*", "?")
    for (ch in chars) {
        x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
    }
    x
}

#' Fix DESCRIPTION File
#'
#' Automatically fixes common CRAN issues in DESCRIPTION.
#'
#' @param path Path to package root directory
#' @param backup Create backup file? Default TRUE.
#' @return Invisibly returns TRUE if changes were made
#'
#' @export
fix_description_cran <- function (path = ".", backup = TRUE) {
    desc_file <- file.path(path, "DESCRIPTION")
    if (!file.exists(desc_file)) {
        stop("No DESCRIPTION file found in ", path, call. = FALSE)
    }

    # Read current content
    desc <- read.dcf(desc_file)
    dep_packages <- get_dependency_packages(desc)
    check_names <- unique(c(dep_packages, KNOWN_SOFTWARE_NAMES))

    made_changes <- FALSE

    # Fix Title
    if ("Title" %in% colnames(desc)) {
        old_title <- desc[1, "Title"]
        new_title <- quote_names_in_text(old_title, check_names)
        if (new_title != old_title) {
            desc[1, "Title"] <- new_title
            made_changes <- TRUE
            message("Fixed Title: quoted package/software names")
        }
    }

    # Fix Description
    if ("Description" %in% colnames(desc)) {
        old_desc <- desc[1, "Description"]
        new_desc <- quote_names_in_text(old_desc, check_names)
        if (new_desc != old_desc) {
            desc[1, "Description"] <- new_desc
            made_changes <- TRUE
            message("Fixed Description: quoted package/software names")
        }
    }

    if (made_changes) {
        if (backup) {
            backup_file <- paste0(desc_file, ".bak")
            file.copy(desc_file, backup_file, overwrite = TRUE)
            message("Backup saved to ", backup_file)
        }

        write.dcf(desc, desc_file)
        message("Updated ", desc_file)
    } else {
        message("No CRAN compliance issues to fix in DESCRIPTION")
    }

    invisible(made_changes)
}

# ============================================================================
# Code Checking Functions
# ============================================================================

#' Check R Code for CRAN Issues
#'
#' Scans R files for common CRAN policy violations.
#'
#' @param path Path to package root directory
#' @return List with issues found
#'
#' @export
check_code_cran <- function (path = ".") {
    r_dir <- file.path(path, "R")
    if (!dir.exists(r_dir)) {
        stop("No R/ directory found in ", path, call. = FALSE)
    }

    r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE,
        ignore.case = TRUE)

    if (length(r_files) == 0) {
        message("No R files found")
        return(invisible(list()))
    }

    all_issues <- list()

    for (file in r_files) {
        lines <- readLines(file, warn = FALSE)
        file_issues <- check_code_lines(lines, basename(file))
        if (length(file_issues) > 0) {
            all_issues[[basename(file)]] <- file_issues
        }
    }

    # Report issues
    if (length(all_issues) > 0) {
        for (fname in names(all_issues)) {
            for (issue in all_issues[[fname]]) {
                warning("CRAN [", fname, ":", issue$line, "]: ", issue$message,
                    call. = FALSE)
            }
        }
    }

    invisible(all_issues)
}

#' Check Code Lines for Issues
#'
#' @param lines Character vector of code lines
#' @param filename Filename for reporting
#' @return List of issues
check_code_lines <- function (lines, filename) {
    issues <- list()

    for (i in seq_along(lines)) {
        line <- lines[i]

        # Skip comments
        if (grepl("^\\s*#", line)) next

        # Check for T/F instead of TRUE/FALSE
        # Match T or F as standalone tokens (word boundaries)
        if (grepl("(^|[^A-Za-z0-9_.])T($|[^A-Za-z0-9_.])", line) ||
            grepl("(^|[^A-Za-z0-9_.])F($|[^A-Za-z0-9_.])", line)) {
            # Exclude common false positives like T.test, F.stat, etc
            if (!grepl("\\bT\\.", line) && !grepl("\\bF\\.", line) &&
                !grepl("\".*[TF].*\"", line) && !grepl("'.*[TF].*'", line)) {
                issues <- c(issues, list(list(
                            line = i,
                            message = "Use TRUE/FALSE instead of T/F"
                        )))
            }
        }

        # Check for print()/cat() outside of print methods
        if (grepl("\\b(print|cat)\\s*\\(", line)) {
            # Skip if it's a print method definition
            if (!grepl("print\\.[A-Za-z]", line) && !grepl("#.*print", line)) {
                issues <- c(issues, list(list(
                            line = i,
                            message = "Avoid print()/cat() - use message() or verbose parameter"
                        )))
            }
        }

        # Check for installed.packages()
        if (grepl("\\binstalled\\.packages\\s*\\(", line)) {
            issues <- c(issues, list(list(
                        line = i,
                        message = "Avoid installed.packages() - use requireNamespace() instead"
                    )))
        }

        # Check for .GlobalEnv
        if (grepl("\\.GlobalEnv", line)) {
            issues <- c(issues, list(list(
                        line = i,
                        message = "Avoid modifying .GlobalEnv"
                    )))
        }

        # Check for options(warn = -1)
        if (grepl("options\\s*\\(\\s*warn\\s*=\\s*-", line)) {
            issues <- c(issues, list(list(
                        line = i,
                        message = "Avoid options(warn = -1) - use suppressWarnings() instead"
                    )))
        }

        # Check for setwd() without on.exit
        if (grepl("\\bsetwd\\s*\\(", line)) {
            # Look for on.exit in nearby lines
            context_start <- max(1, i - 5)
            context_end <- min(length(lines), i + 5)
            context <- paste(lines[context_start:context_end], collapse = "\n")
            if (!grepl("on\\.exit", context)) {
                issues <- c(issues, list(list(
                            line = i,
                            message = "setwd() should be restored with on.exit()"
                        )))
            }
        }

        # Check for hardcoded set.seed without parameter
        if (grepl("\\bset\\.seed\\s*\\(\\s*[0-9]+\\s*\\)", line)) {
            # Check if it's in a function with seed parameter
            # Simple heuristic: look for seed parameter in recent lines
            context_start <- max(1, i - 20)
            context <- paste(lines[context_start:i], collapse = "\n")
            if (!grepl("seed\\s*=", context)) {
                issues <- c(issues, list(list(
                            line = i,
                            message = "Hardcoded set.seed() - consider adding seed parameter"
                        )))
            }
        }
    }

    issues
}

#' Full CRAN Compliance Check
#'
#' Runs all CRAN compliance checks (DESCRIPTION + code).
#'
#' @param path Path to package root directory
#' @return List with all issues
#'
#' @export
check_cran <- function (path = ".") {
    message("Checking CRAN compliance...")

    desc_result <- check_description_cran(path)
    code_result <- check_code_cran(path)

    all_issues <- list(
        description = desc_result$issues,
        code = code_result
    )

    has_issues <- desc_result$has_issues || length(code_result) > 0

    if (!has_issues) {
        message("No CRAN compliance issues found")
    }

    invisible(all_issues)
}

