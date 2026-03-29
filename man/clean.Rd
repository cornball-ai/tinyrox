% tinyrox says don't edit this manually, but it can't stop you!
\name{clean}
\alias{clean}
\title{Clean Generated Files}
\usage{
clean(path = ".", namespace = FALSE)
}
\arguments{
\item{path}{Path to package root directory.}

\item{namespace}{Also remove NAMESPACE? Default FALSE.}
}
\value{
No return value, called for side effects.
}
\description{
Removes all Rd files from man/ directory.
}
\examples{
\donttest{
# Create a minimal package in tempdir
pkg <- file.path(tempdir(), "mypkg")
dir.create(file.path(pkg, "man"), recursive = TRUE, showWarnings = FALSE)
writeLines("Package: mypkg\nTitle: Test\nVersion: 0.1.0",
    file.path(pkg, "DESCRIPTION"))
writeLines("placeholder", file.path(pkg, "man", "test.Rd"))

clean(pkg)

# Clean up
unlink(pkg, recursive = TRUE)
}
}
