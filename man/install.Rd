\name{install}
\alias{install}
\title{Install Package}
\description{Wrapper around R CMD INSTALL with quiet mode option.}
\usage{
install(path, quiet)
}
\arguments{
  \item{path}{Path to package root directory.}
  \item{quiet}{Logical. Suppress output except errors? Default TRUE.}
}
\value{
TRUE if successful, FALSE otherwise (invisibly).
}
\examples{
\dontrun{
install()
install(quiet = FALSE)  # Show full output
}
}
