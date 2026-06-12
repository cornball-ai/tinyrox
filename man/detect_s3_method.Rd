% tinyrox says don't edit this manually, but it can't stop you!
\name{detect_s3_method}
\alias{detect_s3_method}
\title{Detect S3 Method from Function Name}
\usage{
detect_s3_method(name, pkg_generics = character())
}
\arguments{
\item{name}{Function name to check.}

\item{pkg_generics}{Character vector of S3 generics defined in the
package itself, checked in addition to KNOWN_S3_GENERICS.}
}
\value{
List with generic and class components, or NULL if not an S3 method.
}
\description{
Checks if a function name follows the generic.class pattern where
generic is a known S3 generic function.
}
\keyword{internal}
