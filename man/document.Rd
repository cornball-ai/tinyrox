\name{document}
\alias{document}
\title{Generate Documentation for an R Package}
\description{Main function for rhydrogen. Parses R source files for documentation comments and generates Rd files and NAMESPACE.}
\usage{
document(path, namespace)
}
\arguments{
  \item{path}{Path to package root directory. Default is current directory.}
  \item{namespace}{How to handle NAMESPACE generation. One of: - "overwrite": Fully regenerate NAMESPACE (default) - "append": Insert between ## rhydrogen start/end markers - "none": Don't modify NAMESPACE}
}
\value{
Invisibly returns a list with:
  - rd_files: character vector of generated Rd file paths
  - namespace: path to NAMESPACE file (or NULL if mode="none")
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
