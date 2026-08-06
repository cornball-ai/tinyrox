% tinyrox says don't edit this manually, but it can't stop you!
\name{check_rd_filenames}
\alias{check_rd_filenames}
\title{Refuse to Leave a File Name R CMD build Will Drop}
\usage{
check_rd_filenames(files)
}
\arguments{
\item{files}{Character vector of paths just written.}
}
\value{
invisible(TRUE)
}
\description{
rd_filename() is supposed to make every name portable. This says so
out loud rather than trusting it, because the failure is silent in
both directions: `R CMD build` drops the file without an error, and
the `R CMD check` that follows examines a tarball the file is no
longer in.
}
\keyword{internal}
