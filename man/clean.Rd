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
\description{
Removes all Rd files from man/ directory.
}
\examples{
\dontrun{
clean()
clean(namespace = TRUE)
}
}
