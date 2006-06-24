\name{polar.plot}
\title{Plot values on a circular grid of 0 to 360 degrees.}
\usage{
 polar.plot(lengths,polar.pos,labels,label.pos,rp.type="r",...)
}
\alias{polar.plot}
\arguments{
 \item{lengths}{numeric data vector. Magnitudes will be represented as
  line lengths.}
 \item{polar.pos}{numeric vector of positions on a 0:360 degree circle.
  These will be rescaled to radians.}
 \item{labels}{text labels to place on the periphery of the circle. This 
  defaults to labels every 20 degrees. For no labels, pass an empty string.}
 \item{label.pos}{positions of the labels in degrees}
 \item{rp.type}{Whether to plot radial lines, symbols or a polygon.}
 \item{...}{additional arguments passed to \code{radial.plot} and
  then to \code{plot}.}
}
\description{
 \code{polar.plot} displays a plot of radial lines, symbols or a polygon 
  centered at the midpoint of the plot frame on a 0:360 circle.
  Positions are interpreted as beginning at the right and
  moving counterclockwise.}
}
\value{nil}
\author{Jim Lemon}
\keyword{misc}
\seealso{\code{\link{radial.plot}}}
\examples{
 testlen<-c(rnorm(36)*2+5)
 testpos<-seq(0,350,by=10)
 polar.plot(testlen,testpos,main="Test Polar Plot",lwd=3,line.col=4)
}
