% tinyrox says don't edit this manually, but it can't stop you!
\name{order_params}
\alias{order_params}
\title{Order @param Names by the Function's Formals}
\usage{
order_params(param_names, formal_names)
}
\arguments{
\item{param_names}{Character vector of @param names.}

\item{formal_names}{Character vector of formal argument names.}
}
\value{
param_names, reordered.
}
\description{
Formals order first, then anything matching no formal in the order
written. A joined name ("a,b") sorts at its earliest formal.
}
\keyword{internal}
