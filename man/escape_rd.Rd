% tinyrox says don't edit this manually, but it can't stop you!
\name{escape_rd}
\alias{escape_rd}
\title{Escape Special Characters for Rd}
\usage{
escape_rd(text)
}
\arguments{
\item{text}{Text to escape.}
}
\value{
Escaped text.
}
\description{
Escapes special characters for Rd format, but detects and preserves
existing Rd markup (like \\describe{}, \\item{}, \\code{}, etc.).
}
\keyword{internal}
