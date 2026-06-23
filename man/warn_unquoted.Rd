% tinyrox says don't edit this manually, but it can't stop you!
\name{warn_unquoted}
\alias{warn_unquoted}
\title{Warn About Unquoted Names in a DESCRIPTION Field}
\usage{
warn_unquoted(field, found)
}
\arguments{
\item{field}{Field name for the message (e.g. "Title", "Description").}

\item{found}{Character vector of unquoted names detected in the field.}
}
\description{
Splits found names into two confidence tiers and warns accordingly.
Curated software/company names (KNOWN_SOFTWARE_NAMES) are unambiguous
proper nouns, so they get a directive warning. Dependency names matched
in prose may just be ordinary words (e.g. "graphics system"), so they get
a softened, advisory warning that leaves the call to the user.
}
\keyword{internal}
