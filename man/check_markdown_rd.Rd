% tinyrox says don't edit this manually, but it can't stop you!
\name{check_markdown_rd}
\alias{check_markdown_rd}
\title{Warn About Markdown That Ships Verbatim}
\usage{
check_markdown_rd(path = ".")
}
\arguments{
\item{path}{Path to package root directory}
}
\value{
Named list, one integer vector of line numbers per affected
  file (invisibly); empty when nothing was found or the check is off.
}
\description{
tinyrox does no markdown parsing, so backticked text and bracketed
function links written in the roxygen2 markdown habit reach the Rd
unchanged and render literally in help pages and the PDF manual.
R CMD check says nothing about them. This reports each affected file
once. Set options(tinyrox.markdown_check = FALSE) to silence it for a
package that uses backticks on purpose.
}
\keyword{internal}
