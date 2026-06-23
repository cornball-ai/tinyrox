# Token-based CRAN code checking. Scans utils::getParseData() tokens
# instead of raw source lines, so comments and string literals can never
# trigger findings, and function names match whole tokens (torch.cat() is
# not cat()).

#' Check R Code for CRAN Issues
#'
#' Scans R files for common CRAN policy violations. Files are parsed and
#' checked token-wise: comments and string literals are never flagged,
#' print()/cat() are allowed inside print./format. S3 methods, and T/F
#' shorthand is not confused with a local variable named T or F.
#'
#' @param path Path to package root directory
#' @return List with issues found
#'
#' @export
#' @examples
#' \donttest{
#' # Create a minimal package in tempdir
#' pkg <- file.path(tempdir(), "mypkg")
#' dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
#' writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0",
#'     file.path(pkg, "DESCRIPTION"))
#' writeLines("add <- function(x, y) x + y",
#'     file.path(pkg, "R", "add.R"))
#'
#' check_code_cran(pkg)
#'
#' # Clean up
#' unlink(pkg, recursive = TRUE)
#' }
check_code_cran <- function(path = ".") {
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
        file_issues <- check_code_file(file)
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

#' Check One R File for CRAN Issues
#'
#' Parses the file and runs the token-level checks. A file that does not
#' parse is reported as a single issue rather than stopping the whole check.
#'
#' @param file Path to an R source file.
#' @return List of issues (each a list with line and message).
#' @keywords internal
check_code_file <- function(file) {
    parsed <- tryCatch(parse(file, keep.source = TRUE), error = function(e) e)

    if (inherits(parsed, "error")) {
        return(list(list(line = NA_integer_,
                         message = paste0("File does not parse: ",
                        conditionMessage(parsed)))))
    }

    pd <- utils::getParseData(parsed)
    if (is.null(pd) || nrow(pd) == 0) {
        return(list())
    }

    check_code_tokens(pd)
}

#' Run Token-Level CRAN Checks on Parse Data
#'
#' @param pd Parse data from utils::getParseData() with source kept (the
#'   srcfile attribute is needed to reconstruct call text).
#' @return List of issues, sorted by line.
#' @keywords internal
check_code_tokens <- function(pd) {
    issues <- list()
    add_issue <- function(line, message) {
        issues[[length(issues) + 1]] <<- list(line = line, message = message)
    }

    parents <- pd$parent
    names(parents) <- as.character(pd$id)

    # Top-level expression each token belongs to
    pd$root <- vapply(pd$id, token_root, numeric(1), parents = parents)

    terms <- pd[pd$terminal,, drop = FALSE]
    terms <- terms[order(terms$line1, terms$col1),, drop = FALSE]

    # Names assigned (or declared as formals) per top-level expression
    assigned <- assigned_names(terms)

    # Name of the function each top-level expression defines (or NA)
    fn_names <- vapply(unique(terms$root), root_function_name, character(1),
                       terms = terms)
    names(fn_names) <- as.character(unique(terms$root))

    # T/F instead of TRUE/FALSE. Only bare SYMBOL tokens: comments, strings,
    # argument names (SYMBOL_SUB), and $/@ member access don't count, and a
    # T or F assigned in the same top-level expression is a variable, not
    # the logical shorthand.
    prev_token <- c("", terms$token[-nrow(terms)])
    is_tf <- terms$token == "SYMBOL" & terms$text %in% c("T", "F") &
    prev_token != "'$'" & prev_token != "'@'"
    for (i in which(is_tf)) {
        root_key <- as.character(terms$root[i])
        if (terms$text[i] %in% assigned[[root_key]]) {
            next
        }
        add_issue(terms$line1[i], "Use TRUE/FALSE instead of T/F")
    }

    # print()/cat() outside print./format. S3 methods
    is_printcat <- terms$token == "SYMBOL_FUNCTION_CALL" &
    terms$text %in% c("print", "cat")
    for (i in which(is_printcat)) {
        fn <- fn_names[[as.character(terms$root[i])]]
        if (!is.na(fn) && grepl("^(print|format)\\.", fn)) {
            next
        }
        add_issue(terms$line1[i],
                  "Avoid print()/cat() - use message() or verbose parameter")
    }

    # installed.packages()
    is_instpkg <- terms$token == "SYMBOL_FUNCTION_CALL" &
    terms$text == "installed.packages"
    for (i in which(is_instpkg)) {
        add_issue(terms$line1[i],
                  "Avoid installed.packages() - use requireNamespace() instead")
    }

    # .GlobalEnv
    is_globalenv <- terms$token == "SYMBOL" & terms$text == ".GlobalEnv"
    for (i in which(is_globalenv)) {
        add_issue(terms$line1[i], "Avoid modifying .GlobalEnv")
    }

    # options(warn = -1)
    is_options <- terms$token == "SYMBOL_FUNCTION_CALL" &
    terms$text == "options"
    for (i in which(is_options)) {
        call_text <- call_expr_text(pd, terms$id[i], parents)
        if (grepl("warn\\s*=\\s*-", call_text)) {
            add_issue(terms$line1[i],
                      "Avoid options(warn = -1) - use suppressWarnings() instead")
        }
    }

    # setwd() without on.exit() in the same top-level expression
    is_setwd <- terms$token == "SYMBOL_FUNCTION_CALL" & terms$text == "setwd"
    onexit_roots <- terms$root[terms$token == "SYMBOL_FUNCTION_CALL" &
        terms$text == "on.exit"]
    for (i in which(is_setwd)) {
        if (terms$root[i] %in% onexit_roots) {
            next
        }
        add_issue(terms$line1[i], "setwd() should be restored with on.exit()")
    }

    # Hardcoded set.seed(<number>) in a function without a seed formal
    is_setseed <- terms$token == "SYMBOL_FUNCTION_CALL" &
    terms$text == "set.seed"
    for (i in which(is_setseed)) {
        call_text <- call_expr_text(pd, terms$id[i], parents)
        if (!grepl("^set\\.seed\\s*\\(\\s*[0-9]+\\s*\\)$", call_text)) {
            next
        }
        root_key <- as.character(terms$root[i])
        root_terms <- terms[terms$root == terms$root[i],, drop = FALSE]
        has_seed_formal <- any(root_terms$token == "SYMBOL_FORMALS" &
                               root_terms$text == "seed")
        if (!has_seed_formal) {
            add_issue(terms$line1[i],
                      "Hardcoded set.seed() - consider adding seed parameter")
        }
    }

    if (length(issues) > 0) {
        issues <- issues[order(vapply(issues, function(x) {
            if (is.na(x$line)) 0L else as.integer(x$line)
        }, integer(1)))]
    }

    issues
}

#' Find the Top-Level Expression Containing a Token
#'
#' @param id Token id from parse data.
#' @param parents Named vector mapping token id to parent id.
#' @return Id of the top-level expression.
#' @keywords internal
token_root <- function(id, parents) {
    repeat {
        p <- parents[[as.character(id)]]
        if (is.null(p) || is.na(p) || p <= 0) {
            return(as.numeric(id))
        }
        id <- p
    }
}

#' Collect Assigned Names per Top-Level Expression
#'
#' A symbol counts as assigned if it is the target of <-, =, or ->, the
#' loop variable of a for(), or a function formal. Used to tell a local
#' variable named T or F apart from the logical shorthand.
#'
#' @param terms Terminal tokens sorted by position, with a root column.
#' @return Named list mapping root id to character vector of names.
#' @keywords internal
assigned_names <- function(terms) {
    result <- list()
    add <- function(root, name) {
        key <- as.character(root)
        result[[key]] <<- c(result[[key]], name)
    }

    n <- nrow(terms)
    for (i in seq_len(n)) {
        tok <- terms$token[i]
        if (tok == "SYMBOL_FORMALS") {
            add(terms$root[i], terms$text[i])
        } else if (tok %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "IN")) {
            # Target symbol is the terminal just before <-, =, or in
            if (i > 1 && terms$token[i - 1] == "SYMBOL") {
                add(terms$root[i], terms$text[i - 1])
            }
        } else if (tok == "RIGHT_ASSIGN") {
            # Target symbol is the terminal just after ->
            if (i < n && terms$token[i + 1] == "SYMBOL") {
                add(terms$root[i], terms$text[i + 1])
            }
        }
    }

    result
}

#' Name of the Function a Top-Level Expression Defines
#'
#' Recognizes the pattern name <- function(...) (also = and quoted or
#' backticked names). Anything else returns NA.
#'
#' @param root_id Top-level expression id.
#' @param terms Terminal tokens sorted by position, with a root column.
#' @return Function name as a string, or NA.
#' @keywords internal
root_function_name <- function(root_id, terms) {
    rt <- terms[terms$root == root_id,, drop = FALSE]
    if (nrow(rt) >= 3 &&
        rt$token[1] %in% c("SYMBOL", "STR_CONST") &&
        rt$token[2] %in% c("LEFT_ASSIGN", "EQ_ASSIGN") &&
        rt$token[3] == "FUNCTION") {
        return(gsub("^[`\"']|[`\"']$", "", rt$text[1]))
    }
    NA_character_
}

#' Reconstruct the Source Text of a Call
#'
#' Given the SYMBOL_FUNCTION_CALL token id, climbs two levels to the call
#' expression and returns its source text.
#'
#' @param pd Full parse data (srcfile attribute required).
#' @param id Token id of the SYMBOL_FUNCTION_CALL.
#' @param parents Named vector mapping token id to parent id.
#' @return Call source text, or "" if unavailable.
#' @keywords internal
call_expr_text <- function(pd, id, parents) {
    name_expr <- parents[[as.character(id)]]
    if (is.null(name_expr) || is.na(name_expr) || name_expr <= 0) {
        return("")
    }
    call_expr <- parents[[as.character(name_expr)]]
    if (is.null(call_expr) || is.na(call_expr) || call_expr <= 0) {
        return("")
    }
    text <- tryCatch(utils::getParseText(pd, call_expr), error = function(e) "")
    paste(text, collapse = "\n")
}
