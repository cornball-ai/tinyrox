% tinyrox says don't edit this manually, but it can't stop you!
\name{find_package_generics}
\alias{find_package_generics}
\title{Find S3 generics defined in the package}
\usage{
find_package_generics(blocks)
}
\arguments{
\item{blocks}{Documentation blocks from parse_package().}
}
\value{
Character vector of generic function names.
}
\description{
Scans source files for functions that call UseMethod() to identify
package-defined S3 generics.
}
\keyword{internal}
