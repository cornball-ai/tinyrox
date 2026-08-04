% tinyrox says don't edit this manually, but it can't stop you!
\name{format_usage}
\alias{format_usage}
\title{Format Usage Line}
\usage{
format_usage(name, args, pkg_generics = character())
}
\arguments{
\item{name}{Function name.}

\item{args}{Character vector of arguments with defaults.}

\item{pkg_generics}{Character vector of S3 generics defined in the
package itself, used for S3 method detection.}
}
\value{
Formatted usage string.
}
\description{
Formats the function usage, wrapping to multiple lines if needed.
Follows roxygen2: single line when the bare call is under 80 characters,
otherwise one argument per line at two-space indent, with each overlong
argument wrapped to the 90-character Rd limit. S3 methods use the
\\method syntax.
}
\keyword{internal}
