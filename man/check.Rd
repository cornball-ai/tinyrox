% tinyrox says don't edit this manually, but it can't stop you!
\name{check}
\alias{check}
\title{Check Package}
\usage{
check(
  path = ".",
  args = c("--as-cran", "--no-manual"),
  error_on = c("warning", "error", "note")
)
}
\arguments{
\item{path}{Path to package root directory.}

\item{args}{Character vector of additional arguments to pass to R CMD check.
Default includes "--as-cran" and "--no-manual".}

\item{error_on}{Severity level that causes an error: "error", "warning", or
"note". Default is "warning" (fails on errors or warnings).}
}
\value{
TRUE if check passes, FALSE otherwise (invisibly).
  Also throws an error if check fails at or above error_on level.
}
\description{
Runs R CMD build and R CMD check on the package.
}
\examples{
\dontrun{
check()
check(error_on = "error")  # Only fail on errors, not warnings
check(args = c("--as-cran", "--no-manual"))
}
}
