% tinyrox says don't edit this manually, but it can't stop you!
\name{wrap_usage_arg}
\alias{wrap_usage_arg}
\title{Wrap a Single Usage Argument}
\usage{
wrap_usage_arg(x, width = 90L, indent = 4L)
}
\arguments{
\item{x}{Argument string, including its two-space usage indent.}

\item{width}{Maximum line width.}

\item{indent}{Number of spaces prefixed to continuation lines.}
}
\value{
Wrapped string, possibly containing newlines.
}
\description{
Greedy space-fill wrap replicating roxygen2's wrapUsage(): once a line
exceeds the width, the last space outside a quoted string becomes a
newline, and continuation lines get a fixed indent. Quoted strings are
never broken.
}
\keyword{internal}
