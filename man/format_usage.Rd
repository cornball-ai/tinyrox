% tinyrox says don't edit this manually, but it can't stop you!
\name{format_usage}
\alias{format_usage}
\title{Format Usage Line}
\usage{
format_usage(name, args)
}
\arguments{
\item{name}{Function name.}

\item{args}{Character vector of arguments with defaults.}
}
\value{
Formatted usage string.
}
\description{
Formats the function usage, wrapping to multiple lines if needed.
Follows roxygen2 style: each argument on its own line if total > 80 chars.
S3 methods use the \\method syntax.
}
\keyword{internal}
