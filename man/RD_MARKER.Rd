% tinyrox says don't edit this manually, but it can't stop you!
\docType{data}
\name{RD_MARKER}
\alias{RD_MARKER}
\title{Header Marker for tinyrox-Generated Rd Files}
\format{
An object of class \code{character} of length 1.
}
\usage{
RD_MARKER
}
\description{
The first line of every Rd file tinyrox writes. It both warns against hand
editing and lets tinyrox recognise files it owns (see tinyrox_owns_rd()),
so document() can prune only its own stale pages.
}
\keyword{internal}
