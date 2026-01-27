% tinyrox says don't edit this manually, but it can't stop you!
\name{format_object_info}
\alias{format_object_info}
\title{Format Object Info for Data Documentation}
\usage{
format_object_info(name, pkg_path)
}
\arguments{
\item{name}{Object name.}

\item{pkg_path}{Package root path.}
}
\value{
Format string or NULL if object cannot be inspected.
}
\description{
Generates the format description string for a data object.
Tries to load the package namespace to inspect the object.
}
\keyword{internal}
