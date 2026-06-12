% tinyrox says don't edit this manually, but it can't stop you!
\name{root_function_name}
\alias{root_function_name}
\title{Name of the Function a Top-Level Expression Defines}
\usage{
root_function_name(root_id, terms)
}
\arguments{
\item{root_id}{Top-level expression id.}

\item{terms}{Terminal tokens sorted by position, with a root column.}
}
\value{
Function name as a string, or NA.
}
\description{
Recognizes the pattern name <- function(...) (also = and quoted or
backticked names). Anything else returns NA.
}
\keyword{internal}
