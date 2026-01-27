% tinyrox says don't edit this manually, but it can't stop you!
\name{generate_package_rd}
\alias{generate_package_rd}
\title{Generate Package Documentation Rd}
\usage{
generate_package_rd(tags, pkg_name, source_file)
}
\arguments{
\item{tags}{Parsed tags from the documentation block.}

\item{pkg_name}{Package name.}

\item{source_file}{Source file path.}
}
\value{
Character string of Rd content.
}
\description{
Generates Rd content for package documentation ("_PACKAGE" directive).
}
\keyword{internal}
