\name{parse_object_definition}
\alias{parse_object_definition}
\title{Parse Object Definition}
\description{Identifies the object being defined from code text (may be multi-line).}
\usage{
parse_object_definition(text, file, line_num)
}
\arguments{
  \item{text}{The code text (may span multiple lines).}
  \item{file}{The source file (for error messages).}
  \item{line_num}{The line number (for error messages).}
}
\value{
A list with name, type, and formals, or NULL if not a definition.
}
\keyword{internal}
