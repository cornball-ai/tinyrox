#' Known S3 Generic Functions
#'
#' List of base R S3 generics for auto-detection when @export is used.
#' @keywords internal
KNOWN_S3_GENERICS <- c(
    # Print/display
    "print", "format", "summary", "str",
    # Coercion
    "as.character", "as.data.frame", "as.list", "as.matrix", "as.vector",
    "as.numeric", "as.integer", "as.logical", "as.double", "as.complex",
    "as.Date", "as.POSIXct", "as.POSIXlt", "as.factor",
    # Type checking
    "is.na", "is.null", "is.finite", "is.infinite", "is.nan",
    # Subsetting
    "[", "[[", "$", "[<-", "[[<-", "$<-",
    # Math/ops
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
generate_namespace <- function (blocks) {
    exports <- character()
    s3methods <- list()
    imports <- character()
    import_froms <- list()
    use_dynlibs <- character()

    for (block in blocks) {
        tags <- parse_tags(
            block$lines,
            block$object,
            block$file,
            block$line
        )

        # Check for S3 method pattern in exports
        if (tags$export) {
            s3_info <- detect_s3_method(block$object)
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
                s3methods <- c(s3methods, list(list(
                            generic = s3m$generic,
                            class = s3m$class
                        )))
            } else if (!is.null(s3m$explicit)) {
                # Try to parse from function name: generic.class
                parts <- strsplit(block$object, "\\.") [[1]]
                if (length(parts) >= 2) {
                    s3methods <- c(s3methods, list(list(
                                generic = parts[1],
                                class = paste(parts[- 1], collapse = ".")
                            )))
                }
            }
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
        lines <- c(lines, paste0("export(", exp, ")"))
    }

    # S3 methods (sorted by generic, then class)
    if (length(s3methods) > 0) {
        if (length(exports) > 0) lines <- c(lines, "")
        s3methods <- s3methods[order(
            vapply(s3methods, function (x) paste(x$generic, x$class), character(1))
        )]
        for (s3m in s3methods) {
            lines <- c(lines, paste0("S3method(", s3m$generic, ",", s3m$class, ")"))
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

        if (length(imports) == 0) lines <- c(lines, "")
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
write_namespace <- function(
    content,
    path = ".",
    mode = "overwrite"
) {
    filepath <- file.path(path, "NAMESPACE")

    if (mode == "overwrite") {
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
                    after <- existing[(end_pos[1] + 1) :length(existing)]
                } else {
                    after <- character()
                }

                new_content <- c(
                    before,
                    start_marker,
                    strsplit(content, "\n") [[1]],
                    end_marker,
                    after
                )
            } else {
                # No markers - append at end
                new_content <- c(
                    existing,
                    "",
                    start_marker,
                    strsplit(content, "\n") [[1]],
                    end_marker
                )
            }
        } else {
            # New file
            new_content <- c(
                start_marker,
                strsplit(content, "\n") [[1]],
                end_marker
            )
        }

        writeLines(new_content, filepath, useBytes = TRUE)
    }

    filepath
}

#' Detect S3 Method from Function Name
#'
#' Checks if a function name follows the generic.class pattern where
#' generic is a known S3 generic function.
#'
#' @param name Function name to check.
#' @return List with generic and class components, or NULL if not an S3 method.
#' @keywords internal
detect_s3_method <- function(name) {
    # Must contain a dot
    if (!grepl("\\.", name)) {
        return(NULL)
    }

    # Try progressively longer generic names
    # e.g., for "as.data.frame.foo", try "as", "as.data", "as.data.frame"
    parts <- strsplit(name, "\\.") [[1]]

    for (i in seq_len(length(parts) - 1)) {
        generic <- paste(parts[1:i], collapse = ".")
        class <- paste(parts[(i + 1) :length(parts)], collapse = ".")

        if (generic %in% KNOWN_S3_GENERICS) {
            return(list(generic = generic, class = class))
        }
    }

    NULL
}

