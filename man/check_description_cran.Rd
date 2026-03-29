% tinyrox says don't edit this manually, but it can't stop you!
\name{check_description_cran}
\alias{check_description_cran}
\title{Check DESCRIPTION for CRAN Compliance}
\usage{
check_description_cran(path = ".", fix = FALSE)
}
\arguments{
\item{path}{Path to package root directory}

\item{fix}{If TRUE, return fixed text. If FALSE, just warn.}
}
\value{
List with validation results and optionally fixed text
}
\description{
Validates Title and Description fields for common CRAN issues:
unquoted package names, missing web service links, etc.
}
\examples{
\donttest{
# Create a minimal package in tempdir
pkg <- file.path(tempdir(), "mypkg")
dir.create(pkg, recursive = TRUE, showWarnings = FALSE)
writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0\nDescription: A test package.",
    file.path(pkg, "DESCRIPTION"))

check_description_cran(pkg)

# Clean up
unlink(pkg, recursive = TRUE)
}
}
