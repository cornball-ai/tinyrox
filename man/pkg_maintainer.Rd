% tinyrox says don't edit this manually, but it can't stop you!
\name{pkg_maintainer}
\alias{pkg_maintainer}
\title{Get Package Maintainer}
\usage{
pkg_maintainer(path = ".")
}
\arguments{
\item{path}{Path to package root directory.}
}
\value{
A list with elements `name` and `email`.
}
\description{
Extracts pkg_maintainer name and email from DESCRIPTION.
}
\examples{
\dontrun{
pkg_maintainer()
}
}
