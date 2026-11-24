% tinyrox says don't edit this manually, but it can't stop you!
\name{find_markdown_lines}
\alias{find_markdown_lines}
\title{Find Documentation Lines Containing Markdown}
\usage{
find_markdown_lines(lines)
}
\arguments{
\item{lines}{Character vector of file lines}
}
\value{
Integer vector of line numbers
}
\description{
Flags backtick pairs and bracketed function links in documentation
prose. Example code is skipped (backticks are R syntax there), as are
blocks tagged noRd (they ship nowhere) and text already inside an Rd
code macro.
}
\keyword{internal}
