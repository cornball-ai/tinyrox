\name{load_all}
\alias{load_all}
\title{Load All Package Code}
\description{Sources all R files in a package for interactive development, without requiring a full install.}
\usage{
load_all(path, quiet)
}
\arguments{
  \item{path}{Path to package root directory.}
  \item{quiet}{Logical. Suppress file sourcing messages? Default TRUE.}
}
\value{
Character vector of sourced files (invisibly).
}
\examples{
\dontrun{
load_all()
load_all(quiet = FALSE)  # Show each file being sourced
}
}
