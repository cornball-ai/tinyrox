% tinyrox says don't edit this manually, but it can't stop you!
\name{find_dontrun_examples}
\alias{find_dontrun_examples}
\title{Find Exported Functions Using dontrun in Examples}
\usage{
find_dontrun_examples(lines)
}
\arguments{
\item{lines}{Character vector of file lines}
}
\value{
Character vector of function names using dontrun
}
\description{
Parses R file content to find @export tags with \\dontrun in @examples.
}
