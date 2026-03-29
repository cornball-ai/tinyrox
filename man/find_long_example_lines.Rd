% tinyrox says don't edit this manually, but it can't stop you!
\name{find_long_example_lines}
\alias{find_long_example_lines}
\title{Find Example Lines Exceeding 100 Characters}
\usage{
find_long_example_lines(lines, filename)
}
\arguments{
\item{lines}{Character vector of file lines}

\item{filename}{Filename for reporting}
}
\value{
Character vector of warnings (file:line format)
}
\description{
Scans @examples blocks for lines that will be truncated in the PDF manual.
}
