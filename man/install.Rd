% tinyrox says don't edit this manually, but it can't stop you!
\name{install}
\alias{install}
\title{Install Package}
\usage{
install(path = ".", quiet = TRUE)
}
\arguments{
\item{path}{Path to package root directory.}

\item{quiet}{Logical. Suppress output except errors? Default TRUE.}
}
\value{
TRUE if successful, FALSE otherwise (invisibly).
}
\description{
Wrapper around R CMD INSTALL with quiet mode option.
}
\examples{
\dontrun{
install()
install(quiet = FALSE)  # Show full output
}
}
