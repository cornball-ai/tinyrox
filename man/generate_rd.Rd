% tinyrox says don't edit this manually, but it can't stop you!
\name{generate_rd}
\alias{generate_rd}
\title{Generate Rd File Content}
\usage{
generate_rd(tags, formals = NULL, source_file = NULL, pkg_generics = character())
}
\arguments{
\item{tags}{Parsed tags from parse_tags().}

\item{formals}{Character vector of formal argument names (for functions).}

\item{source_file}{Source file path (for header comment).}

\item{pkg_generics}{Character vector of S3 generics defined in the
package itself, used for S3 method detection in usage lines.}
}
\value{
Character string of Rd content.
}
\description{
Generate Rd File Content
}
\keyword{internal}
