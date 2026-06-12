% tinyrox says don't edit this manually, but it can't stop you!
\name{call_expr_text}
\alias{call_expr_text}
\title{Reconstruct the Source Text of a Call}
\usage{
call_expr_text(pd, id, parents)
}
\arguments{
\item{pd}{Full parse data (srcfile attribute required).}

\item{id}{Token id of the SYMBOL_FUNCTION_CALL.}

\item{parents}{Named vector mapping token id to parent id.}
}
\value{
Call source text, or "" if unavailable.
}
\description{
Given the SYMBOL_FUNCTION_CALL token id, climbs two levels to the call
expression and returns its source text.
}
\keyword{internal}
