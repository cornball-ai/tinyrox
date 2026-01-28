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
\dontrun{
check_examples_cran()
check_examples_cran("path/to/package")
}
}
