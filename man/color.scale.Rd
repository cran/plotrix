\name{color.scale}
\alias{color.scale}
\title{Linearly transform numeric values into colors.}
\usage{
 color.scale(x,redrange,greenrange,bluerange)
}
\arguments{
 \item{x}{a numeric vector, matrix or data frame}
 \item{redrange,greenrange,bluerange}{color ranges into which 
  to scale \code{x}}
}
\description{
 \code{color.scale} calculates a sequence of colors by a linear
  transformation of the numeric values supplied into the ranges 
  for red, green and blue. If only one number is supplied for a
  color range, that color remains constant for all values of \code{x}.
  If more than two values are supplied, only the first two are used.
  Values for a color range must be between 0 and 1.
}
\note{
 The function is useful for highlighting a numeric dimension or adding
 an extra "dimension" to a plot.
}
\value{A vector or matrix of hexadecimal color values.}
\author{Jim Lemon}
\seealso{\code{\link{rescale}}}
\examples{
 # go from green to red with a constant blue value
 x<-rnorm(20)
 y<-rnorm(20)
 # use y for the color scale
 plot(x,y,col=color.scale(y,c(0,1),c(1,0),0.3),main="Color scale plot",
  pch=16,cex=2)
}
\keyword{misc}
