% tinyrox says don't edit this manually, but it can't stop you!
\name{assigned_names}
\alias{assigned_names}
\title{Collect Assigned Names per Top-Level Expression}
\usage{
assigned_names(terms)
}
\arguments{
\item{terms}{Terminal tokens sorted by position, with a root column.}
}
\value{
Named list mapping root id to character vector of names.
}
\description{
A symbol counts as assigned if it is the target of <-, =, or ->, the
loop variable of a for(), or a function formal. Used to tell a local
variable named T or F apart from the logical shorthand.
}
\keyword{internal}
