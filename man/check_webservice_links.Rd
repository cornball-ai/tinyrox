% tinyrox says don't edit this manually, but it can't stop you!
\name{check_webservice_links}
\alias{check_webservice_links}
\title{Check for Missing Web Service Links}
\usage{
check_webservice_links(description, packages)
}
\arguments{
\item{description}{Description text}

\item{packages}{Package names from dependencies}
}
\value{
Named list of packages missing links (name = URL)
}
\description{
Checks if packages that typically use web services have
corresponding URLs in the Description.
}
