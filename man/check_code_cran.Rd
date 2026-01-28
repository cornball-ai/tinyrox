% tinyrox says don't edit this manually, but it can't stop you!
\name{check_code_cran}
\alias{check_code_cran}
\title{Check R Code for CRAN Issues}
\usage{
check_code_cran(path = ".")
}
\arguments{
\item{path}{Path to package root directory}
}
\value{
List with issues found
}
\description{
Scans R files for common CRAN policy violations.
}
