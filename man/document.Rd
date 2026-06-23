% tinyrox says don't edit this manually, but it can't stop you!
\name{document}
\alias{document}
\title{Generate Documentation for an R Package}
\usage{
document(path = ".", namespace = c("overwrite", "append", "none"),
         cran_check = TRUE, silent = FALSE)
}
\arguments{
\item{path}{Path to package root directory. Default is current directory.}

\item{namespace}{How to handle NAMESPACE generation. One of
\describe{
\item{"overwrite"}{Fully regenerate NAMESPACE (default)}
\item{"append"}{Insert between ## tinyrox start/end markers}
\item{"none"}{Don't modify NAMESPACE}
}}

\item{cran_check}{Run CRAN compliance checks (code issues and missing
examples) and undocumented-parameter warnings. Default TRUE.}

\item{silent}{Operate less verbose without messages. Default FALSE.}
}
\value{
Invisibly returns a list with:
  - rd_files: character vector of generated Rd file paths
  - namespace: path to NAMESPACE file (or NULL if mode="none")
}
\description{
Main function for tinyrox. Parses R source files for documentation
comments and generates Rd files and NAMESPACE.
}
\examples{
\donttest{
# Create a minimal package in tempdir
pkg <- file.path(tempdir(), "mypkg")
dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0",
    file.path(pkg, "DESCRIPTION"))
writeLines(c(
    "#' Add two numbers",
    "#' @param x A number",
    "#' @param y A number",
    "#' @export",
    "add <- function(x, y) x + y"),
    file.path(pkg, "R", "add.R"))

# Document the package
document(pkg, cran_check = FALSE)

# Clean up
unlink(pkg, recursive = TRUE)
}
}
