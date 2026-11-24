% tinyrox says don't edit this manually, but it can't stop you!
\name{documented_params}
\alias{documented_params}
\title{Formals a Set of @param Tags Documents}
\usage{
documented_params(params)
}
\arguments{
\item{params}{Named list of parameter descriptions from parse_tags().}
}
\value{
Character vector of individual formal names.
}
\description{
One @param can document several formals at once ("@param a,b desc",
rendered as one Rd item named "a,b"). The Rd keeps the joined name; the
undocumented-parameter check needs the individual ones.
}
\keyword{internal}
