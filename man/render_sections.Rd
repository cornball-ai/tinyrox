% tinyrox says don't edit this manually, but it can't stop you!
\name{render_sections}
\alias{render_sections}
\title{Render User-Defined @section Blocks to Rd}
\usage{
render_sections(sections)
}
\arguments{
\item{sections}{List of `list(title=, content=)` from parse_tags().}
}
\value{
Character vector of Rd lines (empty when there are no sections).
}
\description{
Emits one Rd section macro per parsed @section tag. Content is
passed through verbatim as Rd (tinyrox does no markdown parsing), matching
how the title-only macros elsewhere treat hand-written Rd markup.
}
\keyword{internal}
