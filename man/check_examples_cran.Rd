% tinyrox says don't edit this manually, but it can't stop you!
\name{check_examples_cran}
\alias{check_examples_cran}
\title{Check for Missing Examples}
\usage{
check_examples_cran(path = ".")
}
\arguments{
\item{path}{Path to package root directory}
}
\value{
Character vector of function names missing examples
}
\description{
Identifies exported functions that lack examples in their documentation.
}
\examples{
\donttest{
# Create a minimal package in tempdir
pkg <- file.path(tempdir(), "mypkg")
dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0",
    file.path(pkg, "DESCRIPTION"))
writeLines("#' Add numbers\n#' @export\nadd <- function(x, y) x + y",
    file.path(pkg, "R", "add.R"))

check_examples_cran(pkg)

# Clean up
unlink(pkg, recursive = TRUE)
}
}
