#' Known S3 Generic Functions
#'
#' List of base R S3 generics for auto-detection when @export is used.
#' @keywords internal
KNOWN_S3_GENERICS <- c(
                       # Print/display
                       "print", "format", "summary", "str",
                       # Coercion
                       "as.array", "as.character", "as.data.frame", "as.list", "as.matrix",
                       "as.vector",
                       "as.numeric", "as.integer", "as.logical", "as.double", "as.complex",
                       "as.Date", "as.POSIXct", "as.POSIXlt", "as.factor",
                       # Type checking
                       "is.na", "is.null", "is.finite", "is.infinite", "is.nan",
                       # Subsetting
                       "[", "[[", "$", "[<-", "[[<-", "$<-",
                       # Arithmetic operators
                       "+", "-", "*", "/", "^", "%%", "%/%",
                       # Comparison operators
                       "==", "!=", "<", "<=", ">=", ">",
                       # Logical operators
                       "&", "|", "!",
                       # Math/ops group generics
                       "mean", "median", "quantile", "range", "sum", "prod", "min", "max",
                       "Math", "Ops", "Summary", "Complex",
                       # Dimensions
                       "length", "dim", "nrow", "ncol", "names", "dimnames", "row.names",
                       "length<-", "dim<-", "names<-", "dimnames<-", "row.names<-",
                       # Model methods
                       "coef", "fitted", "residuals", "predict", "simulate", "update",
                       "vcov", "confint", "logLik", "AIC", "BIC", "nobs", "df.residual",
                       "deviance", "extractAIC", "model.frame", "model.matrix",
                       "anova", "effects", "weights", "variable.names", "case.names",
                       # Plot
                       "plot", "lines", "points", "text", "image", "contour", "persp",
                       "pairs", "hist", "barplot", "boxplot", "dotchart",
                       # Other common
                       "c", "t", "rep", "rev", "sort", "unique", "duplicated", "anyDuplicated",
                       "merge", "split", "cut", "cbind", "rbind", "stack", "unstack",
                       "head", "tail", "within", "transform", "subset", "aggregate",
                       "droplevels", "xtfrm", "labels", "levels", "levels<-",
                       # Connection/IO
                       "open", "close", "flush", "read", "write", "seek", "truncate",
                       # Misc
                       "all.equal", "Negate"
)

#' Generate NAMESPACE Content
#'
#' @param blocks List of documentation blocks from parse_package().
#' @return Character string of NAMESPACE content.
#' @keywords internal
generate_namespace <- function(blocks) {
    exports <- character()
    export_classes <- character()
    s3methods <- list()
    imports <- character()
    import_froms <- list()
    use_dynlibs <- character()

    # Pre-pass: find package-defined S3 generics (functions calling UseMethod)
    pkg_generics <- find_package_generics(blocks)

    for (block in blocks) {
        tags <- parse_tags(block$lines, block$object, block$file, block$line)

        # Check for S3 method pattern in exports
        if (tags$export) {
            s3_info <- detect_s3_method(block$object, pkg_generics)
            if (!is.null(s3_info)) {
                # It's an S3 method - add to s3methods instead of exports
                s3methods <- c(s3methods, list(s3_info))
            } else {
                # Regular export
                exports <- c(exports, block$object)
            }
        }

        # Explicit S3 methods via @exportS3Method
        if (!is.null(tags$exportS3Method)) {
            s3m <- tags$exportS3Method
            if (!is.null(s3m$generic) && !is.null(s3m$class)) {
                s3methods <- c(s3methods, list(list(generic = s3m$generic,
                            class = s3m$class)))
            } else if (is.character(s3m$explicit) &&
                grepl("::", s3m$explicit, fixed = TRUE)) {
                # @exportS3Method pkg::generic: a method for a generic this
                # package does not import. R registers it when pkg loads,
                # so the directive must keep the pkg:: prefix. The class is
                # the object name with "generic." removed.
                prefix <- paste0(sub("^.*::", "", s3m$explicit), ".")
                if (startsWith(block$object, prefix) &&
                    nchar(block$object) > nchar(prefix)) {
                    s3methods <- c(s3methods, list(list(
                                generic = s3m$explicit,
                                class = substring(block$object,
                                    nchar(prefix) + 1L)
                            )))
                } else {
                    warning("@exportS3Method ", s3m$explicit, " on ",
                            block$object, " at ", basename(block$file), ":",
                            block$line, ": the object name does not start ",
                            "with \"", prefix, "\", so the class is ",
                            "unknown; nothing was registered.", call. = FALSE)
                }
            } else if (!is.null(s3m$explicit)) {
                # Infer from the function name. A known or package-defined
                # generic wins (as.data.frame.foo); otherwise split at the
                # first dot.
                s3_info <- detect_s3_method(block$object, pkg_generics)
                parts <- strsplit(block$object, "\\.")[[1]]
                if (!is.null(s3_info)) {
                    s3methods <- c(s3methods, list(s3_info))
                } else if (length(parts) >= 2) {
                    s3methods <- c(s3methods, list(list(
                                generic = parts[1],
                                class = paste(parts[-1], collapse = ".")
                            )))
                } else {
                    warning("@exportS3Method on ", block$object, " at ",
                            basename(block$file), ":", block$line,
                            " names no generic and class, and the object ",
                            "name has no dot to infer them from; nothing ",
                            "was registered.", call. = FALSE)
                }
            }
        }

        # Export classes
        for (cls in tags$exportClasses) {
            export_classes <- c(export_classes, cls)
        }

        # Imports
        for (imp in tags$imports) {
            imports <- c(imports, imp)
        }

        # ImportFrom
        for (impf in tags$importFroms) {
            import_froms <- c(import_froms, list(impf))
        }

        # useDynLib
        if (!is.null(tags$useDynLib)) {
            use_dynlibs <- c(use_dynlibs, tags$useDynLib)
        }
    }

    # Build NAMESPACE content
    lines <- character()
    lines <- c(lines, "# tinyrox says don't edit this manually, but it can't stop you!")
    lines <- c(lines, "")

    # Exports (sorted)
    exports <- sort(unique(exports))
    for (exp in exports) {
        # Quote names that contain special characters (e.g., replacement functions)
        if (grepl("<-", exp, fixed = TRUE) ||
            !grepl("^[a-zA-Z._][a-zA-Z0-9._]*$", exp)) {
            exp_fmt <- paste0('"', exp, '"')
        } else {
            exp_fmt <- exp
        }
        lines <- c(lines, paste0("export(", exp_fmt, ")"))
    }

    # Export classes (sorted)
    export_classes <- sort(unique(export_classes))
    if (length(export_classes) > 0) {
        if (length(exports) > 0) {
            lines <- c(lines, "")
        }
        for (cls in export_classes) {
            lines <- c(lines, paste0("exportClasses(", cls, ")"))
        }
    }

    # S3 methods (sorted by generic, then class)
    if (length(s3methods) > 0) {
        if (length(exports) > 0 || length(export_classes) > 0) {
            lines <- c(lines, "")
        }
        # A block carrying both @export and @exportS3Method registers once
        s3_keys <- vapply(s3methods, function(x) paste(x$generic, x$class),
                          character(1))
        s3methods <- s3methods[!duplicated(s3_keys)]
        s3methods <- s3methods[order(
                                     vapply(s3methods, function(x) paste(x$generic, x$class), character(1))
            )]
        for (s3m in s3methods) {
            gen <- s3m$generic
            cls <- s3m$class
            # pkg::generic stays bare: quoted, R would look for a generic
            # literally named "pkg::generic"
            if (!grepl("^([a-zA-Z][a-zA-Z0-9.]*::)?[a-zA-Z._][a-zA-Z0-9._]*$",
                       gen)) {
                gen <- paste0('"', gen, '"')
            }
            if (!grepl("^[a-zA-Z._][a-zA-Z0-9._]*$", cls)) {
                cls <- paste0('"', cls, '"')
            }
            lines <- c(lines, paste0("S3method(", gen, ",", cls, ")"))
        }
    }

    # Imports (sorted)
    imports <- sort(unique(imports))
    if (length(imports) > 0) {
        lines <- c(lines, "")
        for (imp in imports) {
            lines <- c(lines, paste0("import(", imp, ")"))
        }
    }

    # ImportFrom (sorted by package, then symbol)
    if (length(import_froms) > 0) {
        # Merge by package
        by_pkg <- list()
        for (impf in import_froms) {
            if (is.null(by_pkg[[impf$pkg]])) {
                by_pkg[[impf$pkg]] <- character()
            }
            by_pkg[[impf$pkg]] <- c(by_pkg[[impf$pkg]], impf$symbols)
        }

        if (length(imports) == 0) {
            lines <- c(lines, "")
        }
        for (pkg in sort(names(by_pkg))) {
            syms <- sort(unique(by_pkg[[pkg]]))
            for (sym in syms) {
                lines <- c(lines, paste0("importFrom(", pkg, ",", sym, ")"))
            }
        }
    }

    # useDynLib (sorted)
    use_dynlibs <- sort(unique(use_dynlibs))
    if (length(use_dynlibs) > 0) {
        lines <- c(lines, "")
        for (udl in use_dynlibs) {
            lines <- c(lines, paste0("useDynLib(", udl, ")"))
        }
    }

    paste(lines, collapse = "\n")
}

#' Write NAMESPACE File
#'
#' @param content NAMESPACE content string.
#' @param path Package root path.
#' @param mode Either "overwrite" or "append".
#' @keywords internal
write_namespace <- function(content, path = ".", mode = "overwrite") {
    filepath <- file.path(path, "NAMESPACE")

    if (mode == "overwrite") {
        warn_dropped_directives(filepath, content)
        writeLines(content, filepath, useBytes = TRUE)
    } else if (mode == "append") {
        # Append between markers
        start_marker <- "## tinyrox start"
        end_marker <- "## tinyrox end"

        if (file.exists(filepath)) {
            existing <- readLines(filepath, warn = FALSE)

            # Find marker positions
            start_pos <- grep(paste0("^", start_marker), existing)
            end_pos <- grep(paste0("^", end_marker), existing)

            if (length(start_pos) > 0 && length(end_pos) > 0) {
                # Replace between markers
                if (start_pos[1] > 1) {
                    before <- existing[1:(start_pos[1] - 1)]
                } else {
                    before <- character()
                }
                if (end_pos[1] < length(existing)) {
                    after <- existing[(end_pos[1] + 1):length(existing)]
                } else {
                    after <- character()
                }

                new_content <- c(before, start_marker,
                                 strsplit(content, "\n")[[1]], end_marker,
                                 after)
            } else {
                # No markers - append at end
                new_content <- c(
                                 existing,
                                 "",
                                 start_marker,
                                 strsplit(content, "\n")[[1]],
                                 end_marker
                )
            }
        } else {
            # New file
            new_content <- c(
                             start_marker,
                             strsplit(content, "\n")[[1]],
                             end_marker
            )
        }

        writeLines(new_content, filepath, useBytes = TRUE)
    }

    filepath
}

#' Warn About Dropped NAMESPACE Directives
#'
#' Compares the existing NAMESPACE against newly generated content and warns
#' about directives the regeneration drops. export() and S3method() lines are
#' excluded - those legitimately churn as tags change. Anything else that
#' vanishes (a hand-added useDynLib(), an import() with no backing tag) is
#' load-bearing and the silent drop breaks packages at runtime.
#'
#' @param filepath Path to the existing NAMESPACE file.
#' @param content Newly generated NAMESPACE content string.
#' @keywords internal
warn_dropped_directives <- function(filepath, content) {
    if (!file.exists(filepath)) {
        return(invisible(NULL))
    }

    old <- trimws(readLines(filepath, warn = FALSE))
    old <- old[nzchar(old) & !grepl("^#", old)]
    new <- trimws(strsplit(content, "\n")[[1]])

    dropped <- setdiff(old, new)
    dropped <- dropped[!grepl("^(export|S3method)\\(", dropped)]

    if (length(dropped) > 0) {
        warning("Regenerating NAMESPACE drops directive(s) that have no ",
                "backing tag in R/:\n",
                paste0("  ", dropped, collapse = "\n"),
                "\nIf these are still needed, add the matching tag ",
                "(e.g. \"#' @useDynLib pkg, .registration = TRUE\" above ",
                "a NULL) so document() regenerates them.", call. = FALSE)
    }

    invisible(NULL)
}

#' Detect S3 Method from Function Name
#'
#' Checks if a function name follows the generic.class pattern where
#' generic is a known S3 generic function.
#'
#' @param name Function name to check.
#' @param pkg_generics Character vector of S3 generics defined in the
#'   package itself, checked in addition to KNOWN_S3_GENERICS.
#' @return List with generic and class components, or NULL if not an S3 method.
#' @keywords internal
detect_s3_method <- function(name, pkg_generics = character()) {
    all_generics <- c(KNOWN_S3_GENERICS, pkg_generics)

    # Must contain a dot
    if (!grepl("\\.", name)) {
        return(NULL)
    }

    # Try progressively longer generic names
    # e.g., for "as.data.frame.foo", try "as", "as.data", "as.data.frame"
    parts <- strsplit(name, "\\.")[[1]]

    for (i in seq_len(length(parts) - 1)) {
        generic <- paste(parts[1:i], collapse = ".")
        class <- paste(parts[(i + 1):length(parts)], collapse = ".")

        if (generic %in% all_generics) {
            return(list(generic = generic, class = class))
        }
    }

    NULL
}

#' Find S3 generics defined in the package
#'
#' Scans source files for functions that call UseMethod() to identify
#' package-defined S3 generics.
#'
#' @param blocks Documentation blocks from parse_package().
#' @return Character vector of generic function names.
#' @keywords internal
find_package_generics <- function(blocks) {
    # Collect unique source files from blocks
    files <- unique(vapply(blocks, function(b) b$file, character(1)))
    generics <- character()

    for (f in files) {
        if (!file.exists(f)) {
            next
        }
        lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
        # Find lines with UseMethod("name")
        m <- regmatches(lines, regexpr('UseMethod\\("([^"]+)"\\)', lines))
        for (match in m) {
            # Extract the generic name
            gen <- sub('UseMethod\\("([^"]+)"\\)', "\\1", match)
            generics <- c(generics, gen)
        }
    }

    unique(generics)
}
