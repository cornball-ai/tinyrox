% tinyrox says don't edit this manually, but it can't stop you!
\name{check_description_cran}
\alias{check_description_cran}
\title{Check DESCRIPTION for CRAN Compliance}
\usage{
check_description_cran(path = ".", fix = FALSE)
}
\arguments{
\item{path}{Path to package root directory}

\item{fix}{If TRUE, return fixed text. If FALSE, just warn.}
}
\value{
List with validation results and optionally fixed text
}
\description{
Validates Title and Description fields for common CRAN issues:
unquoted package names, missing web service links, etc.
}
