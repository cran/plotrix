\name{multhist}
\alias{multhist}
\title{Plot a multiple histogram, as a barplot}
\description{
 Given a list, plots a side-by-side barplot containing the histograms
 of the elements
}
\usage{
 multhist(x,beside=TRUE,freq=NULL,probability=!freq, ...)
}
\arguments{
 \item{x}{a list of numeric vectors}
 \item{beside}{plot histogram bars for groups side-by-side?}
 \item{freq}{logical; if 'TRUE', the histogram graphic is a representation
             of frequencies, the 'counts' component of the result; if
             'FALSE', probability densities, component 'density', are
             plotted (so that the histogram has a total area of one).
             Defaults to 'TRUE' if 'probability' is not specified
             (does not consider equidistant breaks as in \code{\link{hist}})}
 \item{probability}{an alias for '!freq', for S compatibility}
 \item{...}{additional arguments to \code{\link{hist}} or \code{\link{barplot}}}
}
\value{
 plots a side-by-side barplot of the histograms
}
\author{Ben Bolker}
\seealso{\code{\link{hist}},\code{\link{barplot}}}
\note{
 The 'inside' argument to \code{\link{barplot}} (which is not currently
  implemented in barplot anyway) is deleted from the argument list. The
  default value of NULL for \code{freq} is for consistency with \code{hist}
  but is equivalent to TRUE.
}
\examples{
 l <- list(runif(10)*10,1:10,c(1,1,1,1,4,8))
 multhist(l)
}
\keyword{hplot}
