% tinyrox says don't edit this manually, but it can't stop you!
\name{escape_percent}
\alias{escape_percent}
\title{Escape Unescaped Percent Signs}
\usage{
escape_percent(text)
}
\arguments{
\item{text}{Text to escape.}
}
\value{
Text with every \% escaped exactly once.
}
\description{
Escapes \% as \\% for Rd, leaving an already-escaped \\% alone. roxygen2
makes authors escape \% themselves, so sources migrated from roxygen2
carry \\% already; escaping unconditionally would double it into \\\\%,
which Rd reads as a literal backslash plus a comment start (#31).
}
\keyword{internal}
