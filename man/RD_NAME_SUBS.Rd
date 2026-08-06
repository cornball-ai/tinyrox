% tinyrox says don't edit this manually, but it can't stop you!
\docType{data}
\name{RD_NAME_SUBS}
\alias{RD_NAME_SUBS}
\title{What a Topic Is Called on Disk}
\format{
An object of class \code{character} of length 32.
}
\usage{
RD_NAME_SUBS
}
\description{
Replacements applied to a topic name to make a file name, longest
key first so `[<-` is read before `[`. This is the table roxygen2
uses, so a package moving between the two does not churn its `man`
directory, and the two names tinyrox already handled -- `\%||\%` and
a leading dot -- come out exactly as they did.
}
\keyword{internal}
