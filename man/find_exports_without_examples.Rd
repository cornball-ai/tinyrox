% tinyrox says don't edit this manually, but it can't stop you!
\name{find_exports_without_examples}
\alias{find_exports_without_examples}
\title{Find Exported Functions Without Examples}
\usage{
find_exports_without_examples(blocks)
}
\arguments{
\item{blocks}{List of documentation blocks from parse_package()}
}
\value{
Character vector of function names missing examples
}
\description{
Examples belong to an Rd page, and blocks sharing an @rdname share one
page, so the check runs per topic: an exported function is reported only
when no block on its page carries examples. A block that generates no
page (a bare @export above an S3 method, or @noRd) is skipped, since
there is nowhere for an example to live.
}
