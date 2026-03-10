% tinyrox says don't edit this manually, but it can't stop you!
\name{resolve_external_params}
\alias{resolve_external_params}
\title{Resolve Parameters from External Package Rd Files}
\usage{
resolve_external_params(source_name)
}
\arguments{
\item{source_name}{Character string like "base::cat" or "stats::lm".}
}
\value{
Named list of parameter descriptions, or empty list on failure.
}
\description{
Reads an installed package's Rd file to extract parameter documentation
for use with `@inheritParams pkg::function`.
}
\keyword{internal}
