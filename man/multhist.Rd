\name{multhist}
\alias{multhist}
\title{Plot a multiple histogram, as a barplot}
\description{
 Given a list, plots a side-by-side barplot containing the histograms
 of the elements
}
\usage{
 multhist(x,breaks="Sturges",...)
}
\arguments{
 \item{x}{a list of numeric vectors}
 \item{breaks}{vector of breakpoints passed to \code{hist}.}
 \item{...}{additional arguments to barplot}
}
\value{
 plots a side-by-side barplot of the histograms
}
\author{Ben Bolker}
\seealso{\code{\link{hist}},\code{\link{barplot}}}
\examples{
 l <- list(runif(10)*10,1:10,c(1,1,1,1,4,8))
 multhist(l)
}
\keyword{hplot}
