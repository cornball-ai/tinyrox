% tinyrox says don't edit this manually, but it can't stop you!
\name{check_code_file}
\alias{check_code_file}
\title{Check One R File for CRAN Issues}
\usage{
check_code_file(file)
}
\arguments{
\item{file}{Path to an R source file.}
}
\value{
List of issues (each a list with line and message).
}
\description{
Parses the file and runs the token-level checks. A file that does not
parse is reported as a single issue rather than stopping the whole check.
}
\keyword{internal}
