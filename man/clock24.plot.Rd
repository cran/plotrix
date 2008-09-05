\name{clock24.plot}
\title{Plot values on a 24 hour "clockface".}
\usage{
 clock24.plot(lengths,clock.pos,labels=NULL,label.pos=NULL,rp.type="r",...)
}
\alias{clock24.plot}
\arguments{
 \item{lengths}{numeric data vector. Magnitudes will be represented as
  line lengths, or symbol or polygon vertex positions.}
 \item{clock.pos}{numeric vector of positions on the 'clockface'.
  These must be in decimal hours and will be rescaled to radians.}
 \item{labels}{Labels to place at the circumference.}
 \item{label.pos}{Radial positions of the labels.}
 \item{rp.type}{Whether to plot radial lines, symbols or a polygon.}
 \item{...}{additional arguments are passed to \code{radial.plot} and
  then to \code{plot}.}
}
\description{
 \code{clock24.plot} displays a plot of radial lines, symbols or a polygon
  centered at the midpoint of the plot frame on a 24 hour 'clockface'. 
  In contrast to the default behavior of \code{radial.plot}, the positions 
  are interpreted as beginning at vertical (000) and moving clockwise.
}
\value{nil}
\keyword{misc}
\author{Jim Lemon}
\seealso{\code{\link{polar.plot}},\code{\link{radial.plot}}}
\examples{
 testlen<-rnorm(24)*2+5
 testpos<-0:23+rnorm(24)/4
 clock24.plot(testlen,testpos,main="Test Clock24 (lines)",show.grid=FALSE,
  line.col="green",lwd=3)
 if(dev.interactive()) par(ask=TRUE)
 # now do a 'daylight' plot
 clock24.plot(testlen[7:19],testpos[7:19],
  main="Test Clock24 daytime (symbols)",
  point.col="blue",rp.type="s",lwd=3)
 # reset the margins
 par(mar=c(5,4,4,2))
}
