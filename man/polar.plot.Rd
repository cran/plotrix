\name{polar.plot}
\title{Plot values on a circular grid of 0 to 360 degrees.}
\usage{
 polar.plot(lengths,polar.pos=NULL,labels,label.pos=NULL,
  start=0,clockwise=FALSE,rp.type="r",...)
}
\alias{polar.plot}
\arguments{
 \item{lengths}{numeric data vector. Magnitudes will be represented as
  the radial positions of symbols, line ends or polygon vertices.}
 \item{polar.pos}{numeric vector of positions on a 0:360 degree circle.
  These will be converted to radians when passed to \code{radial.plot}.}
 \item{labels}{text labels to place on the periphery of the circle. This 
  defaults to labels every 20 degrees. For no labels, pass an empty string.}
 \item{label.pos}{positions of the peripheral labels in degrees}
 \item{start}{The position for zero degrees on the plot in degrees.}
 \item{clockwise}{Whether to increase angles clockwise rather than the
  default counterclockwise.}
 \item{rp.type}{Whether to plot radial lines, symbols or a polygon.}
 \item{...}{additional arguments passed to \code{radial.plot} and
  then to \code{plot}.}
}
\description{
 \code{polar.plot} displays a plot of radial lines, symbols or a polygon 
  centered at the midpoint of the plot frame on a 0:360 circle.
  Positions are interpreted as beginning at the right and moving
  counterclockwise unless \code{start} specifies another starting point or
  \code{clockwise} is TRUE.
}
\value{nil}
\author{Jim Lemon}
\keyword{misc}
\seealso{\code{\link{radial.plot}}}
\examples{
 testlen<-c(rnorm(36)*2+5)
 testpos<-seq(0,350,by=10)
 polar.plot(testlen,testpos,main="Test Polar Plot",lwd=3,line.col=4)
 polar.plot(testlen,testpos,main="Test Clockwise Polar Plot",
  start=90,clockwise=TRUE,lwd=3,line.col=4)
 # reset the margins
 par(mar=c(5,4,4,2))
}
