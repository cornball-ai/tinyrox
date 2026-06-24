% tinyrox says don't edit this manually, but it can't stop you!
\name{prune_stale_rd}
\alias{prune_stale_rd}
\title{Prune Stale tinyrox-Generated Rd Files}
\usage{
prune_stale_rd(path, generated, silent = FALSE)
}
\arguments{
\item{path}{Package root directory.}

\item{generated}{Character vector of Rd file paths produced this run.}

\item{silent}{Suppress the removal message? Default FALSE.}
}
\value{
Character vector of removed file paths (invisibly).
}
\description{
Removes man/*.Rd files that the current document() run did not
(re)generate, but only those tinyrox can prove it owns via
tinyrox_owns_rd(). Hand-written Rd, and any Rd lacking the marker, are
left untouched.
}
\keyword{internal}
