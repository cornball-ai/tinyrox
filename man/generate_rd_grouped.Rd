% tinyrox says don't edit this manually, but it can't stop you!
\name{generate_rd_grouped}
\alias{generate_rd_grouped}
\title{Generate Rd Content for Grouped Blocks (Multiple @rdname Entries)}
\usage{
generate_rd_grouped(topic, entries, all_tags, pkg_generics = character())
}
\arguments{
\item{topic}{Topic name (the @rdname value).}

\item{entries}{List of list(tags, block) pairs sharing this topic.}

\item{all_tags}{All parsed tags (for @inheritParams resolution).}

\item{pkg_generics}{Character vector of S3 generics defined in the
package itself, used for S3 method detection in usage lines.}
}
\value{
Character string of merged Rd content.
}
\description{
Merges multiple documentation blocks that share an @rdname topic
into a single .Rd file. The primary block (whose name matches the
topic) provides title/description; all blocks contribute usage/params.
}
\keyword{internal}
