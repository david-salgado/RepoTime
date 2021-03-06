% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newRepoTime.R
\name{newRepoTime}
\alias{newRepoTime}
\title{Constructor of objects of class \linkS4class{RepoTimeInt}}
\usage{
newRepoTime(Time)
}
\arguments{
\item{Time}{Character vector with the time intervals in the repo notation.}
}
\value{
Object of class \linkS4class{RepoTimeInt}.
}
\description{
This constructor takes as argument a character vector with the 
time intervals in the repo notation and returns the corresponding 
\linkS4class{RepoTimeInt} object.
}
\details{
The repo notation of time intervals is explained in the details 
section of \code{\link{RepoTimeInt-class}}.
}
\examples{
x <- newRepoTime(c('MM042015', 'MM052015')) 
x
str(x) 

# It works even with lists instead of vector
newRepoTime(list('AA2015', 'TT12012'))

}
