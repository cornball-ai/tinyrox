% tinyrox says don't edit this manually, but it can't stop you!
\name{save_tag}
\alias{save_tag}
\title{Save a Parsed Tag Value}
\usage{
save_tag(result, tag, arg, accumulator, file, line_num)
}
\arguments{
\item{result}{Current result list to update.}

\item{tag}{Tag name (e.g., "param", "return").}

\item{arg}{First-line argument after the tag.}

\item{accumulator}{Continuation lines for the tag.}

\item{file}{Source file path (for error messages).}

\item{line_num}{Line number (for error messages).}
}
\description{
Save a Parsed Tag Value
}
\keyword{internal}
