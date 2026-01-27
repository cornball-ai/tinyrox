% tinyrox says don't edit this manually, but it can't stop you!
\name{extract_nn_module_formals}
\alias{extract_nn_module_formals}
\title{Extract Formals from torch nn_module}
\usage{
extract_nn_module_formals(code)
}
\arguments{
\item{code}{Code starting with "nn_module(" or "torch::nn_module("}
}
\value{
List with 'names' (argument names) and 'usage' (formatted for Rd),
  or NULL if initialize method not found.
}
\description{
Extracts the formals from the initialize method of an nn_module definition.
}
\keyword{internal}
