\name{color.scale}
\alias{color.scale}
\title{color.scale}
\usage{
 color.scale(x,redrange,greenrange,bluerange,scale.up=FALSE)
}
\arguments{
 \item{x}{a numeric vector}
 \item{redrange,greenrange,bluerange}
  {color ranges into which to scale the numeric vector \code{x}}
 \item{scale.up}{Whether to scale color values less than or equal 
  to 1 into a 0->255 range. If colors are specified in a 0->1 range, 
  set this to TRUE.}
}
\description{
 \code{color.scale} calculates sequences of three colors by linearly
  interpolating between the values specified for each color. If only
  one number is supplied for a color, that color remains constant for
  all values of \code{x}. If more than two values are supplied, only
  the first two are used. If any values for a color range are less
  than 0 or greater than 255, that range is rescaled into 0->255.}
}
\value{A vector of hexadecimal color values as used by \code{col}.}
\authors{Jim Lemon}
\seealso{\code{\link{rgb.to.hex},\link{rescale}}}
\examples{
 # go from green to red with a constant blue value
 x<-rnorm(20)
 y<-rnorm(20)
 # use y for the color scale
 plot(x,y,col=color.scale(y,c(0,1),c(1,0),0.3,scale.up=TRUE))
}
\keyword{misc}
