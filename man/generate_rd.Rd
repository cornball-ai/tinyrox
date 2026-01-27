% tinyrox says don't edit this manually, but it can't stop you!
\name{generate_rd}
\alias{generate_rd}
\title{Generate Rd File Content}
\usage{
generate_rd(tags, formals = NULL, source_file = NULL)
}
\arguments{
\item{tags}{Parsed tags from parse_tags().}

\item{formals}{Character vector of formal argument names (for functions).}

\item{source_file}{Source file path (for header comment).}
}
\value{
Character string of Rd content.
}
\description{
Generate Rd File Content
}
\keyword{internal}
