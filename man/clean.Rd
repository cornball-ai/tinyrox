\name{clean}
\alias{clean}
\title{Clean Generated Files}
\description{Removes all Rd files from man/ directory.}
\usage{
clean(path, namespace)
}
\arguments{
  \item{path}{Path to package root directory.}
  \item{namespace}{Also remove NAMESPACE? Default FALSE.}
}
\examples{
\dontrun{
clean()
clean(namespace = TRUE)
}
}
