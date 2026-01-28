% tinyrox says don't edit this manually, but it can't stop you!
\name{find_exports_without_examples}
\alias{find_exports_without_examples}
\title{Find Exported Functions Without Examples}
\usage{
find_exports_without_examples(lines)
}
\arguments{
\item{lines}{Character vector of file lines}
}
\value{
Character vector of function names missing examples
}
\description{
Parses R file content to find @export tags without @examples.
}
