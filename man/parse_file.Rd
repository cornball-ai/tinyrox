% tinyrox says don't edit this manually, but it can't stop you!
\name{parse_file}
\alias{parse_file}
\title{Parse R Files for Documentation Blocks}
\usage{
parse_file(file)
}
\arguments{
\item{file}{Path to an R source file.}
}
\value{
A list of documentation blocks, each with components:
  - lines: character vector of comment lines (without #')
  - object: name of the documented object
  - type: "function", "data", or "other"
  - formals: for functions, the formal arguments
}
\description{
Extracts documentation comment blocks and their associated objects
from R source files.
}
\keyword{internal}
