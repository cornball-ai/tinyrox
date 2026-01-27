% tinyrox says don't edit this manually, but it can't stop you!
\name{build}
\alias{build}
\title{Build Package Tarball}
\usage{
build(path = ".", dest_dir = ".")
}
\arguments{
\item{path}{Path to package root directory.}

\item{dest_dir}{Directory to place the tarball. Default is current directory.}
}
\value{
Path to the built tarball (invisibly).
}
\description{
Builds a source package tarball suitable for CRAN submission.
}
\examples{
\dontrun{
build()
build(dest_dir = tempdir())
}
}
