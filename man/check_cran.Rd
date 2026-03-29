% tinyrox says don't edit this manually, but it can't stop you!
\name{check_cran}
\alias{check_cran}
\title{Full CRAN Compliance Check}
\usage{
check_cran(path = ".")
}
\arguments{
\item{path}{Path to package root directory}
}
\value{
List with all issues
}
\description{
Runs all CRAN compliance checks (DESCRIPTION + code).
}
\examples{
\donttest{
# Create a minimal package in tempdir
pkg <- file.path(tempdir(), "mypkg")
dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0\nDescription: A test package.",
    file.path(pkg, "DESCRIPTION"))
writeLines("add <- function(x, y) x + y",
    file.path(pkg, "R", "add.R"))

check_cran(pkg)

# Clean up
unlink(pkg, recursive = TRUE)
}
}
