% tinyrox says don't edit this manually, but it can't stop you!
\name{warn_dropped_directives}
\alias{warn_dropped_directives}
\title{Warn About Dropped NAMESPACE Directives}
\usage{
warn_dropped_directives(filepath, content)
}
\arguments{
\item{filepath}{Path to the existing NAMESPACE file.}

\item{content}{Newly generated NAMESPACE content string.}
}
\description{
Compares the existing NAMESPACE against newly generated content and warns
about directives the regeneration drops. export() and S3method() lines are
excluded - those legitimately churn as tags change. Anything else that
vanishes (a hand-added useDynLib(), an import() with no backing tag) is
load-bearing and the silent drop breaks packages at runtime.
}
\keyword{internal}
