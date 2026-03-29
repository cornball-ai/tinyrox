% tinyrox says don't edit this manually, but it can't stop you!
\name{check_code_cran}
\alias{check_code_cran}
\title{Check R Code for CRAN Issues}
\usage{
check_code_cran(path = ".")
}
\arguments{
\item{path}{Path to package root directory}
}
\value{
List with issues found
}
\description{
Scans R files for common CRAN policy violations.
}
\examples{
\donttest{
# Create a minimal package in tempdir
pkg <- file.path(tempdir(), "mypkg")
dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0",
    file.path(pkg, "DESCRIPTION"))
writeLines("add <- function(x, y) x + y",
    file.path(pkg, "R", "add.R"))

check_code_cran(pkg)

# Clean up
unlink(pkg, recursive = TRUE)
}
}
