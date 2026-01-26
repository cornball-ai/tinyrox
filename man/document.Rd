% tinyrox says don't edit this manually, but it can't stop you!
\name{document}
\alias{document}
\title{Generate Documentation for an R Package}
\usage{
document(path = ".", namespace = c("overwrite", "append", "none"))
}
\arguments{
\item{path}{Path to package root directory. Default is current directory.}

\item{namespace}{How to handle NAMESPACE generation. One of
\describe{
\item{"overwrite"}{Fully regenerate NAMESPACE (default)}
\item{"append"}{Insert between ## tinyrox start/end markers}
\item{"none"}{Don't modify NAMESPACE}
}}
}
\value{
Invisibly returns a list with:
  - rd_files: character vector of generated Rd file paths
  - namespace: path to NAMESPACE file (or NULL if mode="none")
}
\description{
Main function for tinyrox. Parses R source files for documentation
comments and generates Rd files and NAMESPACE.
}
\examples{
\dontrun{
# Document current package
document()

# Document with append mode for NAMESPACE
document(namespace = "append")

# Document without modifying NAMESPACE
document(namespace = "none")
}
}
