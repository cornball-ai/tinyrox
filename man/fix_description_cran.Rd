% tinyrox says don't edit this manually, but it can't stop you!
\name{fix_description_cran}
\alias{fix_description_cran}
\title{Fix DESCRIPTION File}
\usage{
fix_description_cran(path = ".", backup = TRUE)
}
\arguments{
\item{path}{Path to package root directory}

\item{backup}{Create backup file? Default TRUE.}
}
\value{
Invisibly returns TRUE if changes were made
}
\description{
Automatically fixes common CRAN issues in DESCRIPTION.
}
