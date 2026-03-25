% tinyrox says don't edit this manually, but it can't stop you!
\name{parse_tags}
\alias{parse_tags}
\title{Parse Tags from Documentation Lines}
\usage{
parse_tags(lines, object_name, file = NULL, line_num = NULL)
}
\arguments{
\item{lines}{Character vector of documentation lines (without #').}

\item{object_name}{Name of the documented object.}

\item{file}{Source file (for error messages).}

\item{line_num}{Starting line number (for error messages).}
}
\value{
A list with parsed tag values.
}
\description{
Parse Tags from Documentation Lines
}
\examples{
lines <- c("Title Here", "", "Description text.", "", "@param x A number.",
  "@return The number.", "@export")
tags <- parse_tags(lines, "my_function")
tags$title
tags$params

}
