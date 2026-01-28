% tinyrox says don't edit this manually, but it can't stop you!
\name{get_dependency_packages}
\alias{get_dependency_packages}
\title{Extract Package Names from Dependencies}
\usage{
get_dependency_packages(desc)
}
\arguments{
\item{desc}{DESCRIPTION matrix from read.dcf()}
}
\value{
Character vector of package names
}
\description{
Parses Imports, Suggests, Depends, and Enhances fields.
}
