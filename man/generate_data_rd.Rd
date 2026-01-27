% tinyrox says don't edit this manually, but it can't stop you!
\name{generate_data_rd}
\alias{generate_data_rd}
\title{Generate Rd File Content for Data Objects}
\usage{
generate_data_rd(tags, source_file = NULL, format_string = NULL)
}
\arguments{
\item{tags}{Parsed tags from parse_tags().}

\item{source_file}{Source file path (for header comment).}

\item{format_string}{Format description (e.g., "An object of class list of length 3").}
}
\value{
Character string of Rd content.
}
\description{
Generate Rd File Content for Data Objects
}
\keyword{internal}
