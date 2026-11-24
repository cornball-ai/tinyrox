% tinyrox says don't edit this manually, but it can't stop you!
\name{rd_filename}
\alias{rd_filename}
\title{Turn a Topic Name into a Portable File Name}
\usage{
rd_filename(name)
}
\arguments{
\item{name}{Topic name.}
}
\value{
A file name stem, with no .Rd extension.
}
\description{
A topic can be called anything R can name, and \code{$.foo}, \code{[[.foo}
and \code{$<-.foo} are ordinary S3 methods. Those characters are not
legal in a file name on every platform R runs on, and \code{R CMD build}
does not warn and carry on: it prints "excluding invalid files" and
**drops them from the tarball**. The package then installs with no
documentation for those methods, and \code{R CMD check} reports nothing,
because it checks the tarball the files are no longer in. Green,
and missing its docs.
}
\details{
The \code{\\name{}} inside the file keeps the real topic, so help still
resolves under it; only the file on disk is renamed.

}
\keyword{internal}
