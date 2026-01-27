% tinyrox says don't edit this manually, but it can't stop you!
\name{get_maintainer_from_desc}
\alias{get_maintainer_from_desc}
\title{Get Maintainer Info from DESCRIPTION}
\usage{
get_maintainer_from_desc(path)
}
\arguments{
\item{path}{Package root path.}
}
\value{
Formatted author string or NULL.
}
\description{
Reads Authors@R from DESCRIPTION and formats maintainer for Rd.
}
\keyword{internal}
