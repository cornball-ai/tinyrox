% tinyrox says don't edit this manually, but it can't stop you!
\name{check_code_tokens}
\alias{check_code_tokens}
\title{Run Token-Level CRAN Checks on Parse Data}
\usage{
check_code_tokens(pd)
}
\arguments{
\item{pd}{Parse data from utils::getParseData() with source kept (the
srcfile attribute is needed to reconstruct call text).}
}
\value{
List of issues, sorted by line.
}
\description{
Run Token-Level CRAN Checks on Parse Data
}
\keyword{internal}
