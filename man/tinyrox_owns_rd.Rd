% tinyrox says don't edit this manually, but it can't stop you!
\name{tinyrox_owns_rd}
\alias{tinyrox_owns_rd}
\title{Identify a tinyrox-Generated Rd File}
\usage{
tinyrox_owns_rd(file)
}
\arguments{
\item{file}{Path to an Rd file.}
}
\value{
TRUE if tinyrox generated the file, FALSE otherwise.
}
\description{
Returns TRUE only if the file's first line is exactly RD_MARKER. This is
deliberately conservative: hand-written Rd, or Rd written by other tools,
never matches, so prune_stale_rd() cannot delete them.
}
\keyword{internal}
