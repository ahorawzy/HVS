% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_link.R
\name{extract_link}
\alias{extract_link}
\title{Extract links from a path string.}
\usage{
extract_link(path_i)
}
\arguments{
\item{path_i}{A string contains path information and seperated by ','}
}
\value{
A character vector contains all links in this path.
}
\description{
\code{extract_link} can extracts links information from a path string.
}
\examples{
extract_link('1,3,5,6,7,10')

}
