\name{radial.plot}
\title{radial.plot}
\usage{
 radial.plot(lengths,radial.pos,radial.range,labels,label.pos,rp.type="r",
 label.prop=1.1,main="",xlab="",ylab="",...)
}
\alias{radial.plot}
\arguments{
 \item{lengths}{numeric data vector. Magnitudes will be represented as
  line lengths or polygon vertices.}
 \item{radial.pos}{numeric vector of positions in radians. These are
  interpreted as beginning at the right (0 radians) and moving
  counterclockwise.}
 \item{radial.range}{range of positions in radians. This allows a vector
  of lines to be spread over part of the 2*pi range of the plot.}
 \item{labels}{character strings to be placed at the outer ends of
  the lines. If set to NULL, will suppress printing of labels,
  but if missing, the radial positions will be used.}
 \item{label.pos}{the positions of the labels as distance from the
  'center' of the plot. If missing, they will be placed at 1.1
  times the maximum of \code{lengths}.}
 \item{rp.type}{whether to draw radial lines or a polygon.}
 \item{label.prop}{the label position radius as aproportion of the 
  maximum line length.}
 \item{main}{The title for the plot.}
 \item{xlab,ylab}{Normally x and y axis labels are suppressed.}
 \item{...}{additional arguments are passed to \code{plot}.}
}
\description{
 \code{radial.plot} displays a plot of radial lines or a polygon
  centered at the midpoint of the plot frame, the lengths or
  vertices corresponding to the numeric magnitudes of the data values.
}
\value{nil}
\details{The radial.plot family of plots is useful for illustrating
 cyclic data such as wind direction and velocity, activity at different
 times of the day, and so on. While \code{radial.plot} actually does
 the plotting, another function is usually called for specific types
 of cyclic data.}
\authors{Jim Lemon}
\keyword{misc}
\seealso{\code{polar.plot},\code{clock24.plot}}
\examples{
 testlen<-rnorm(10)*2+5
 testpos<-seq(0,18*pi/10,length=10)
 testlab<-letters[1:10]
 radial.plot(testlen,testpos,main="Test Radial Lines")
 par(ask=TRUE)
 testlen<-c(sin(seq(0,1.98*pi,length=100))+2+rnorm(100)/10)
 testpos<-seq(0,1.98*pi,length=100)
 radial.plot(testlen,testpos,rp.type="p",main="Test Polygon",col="blue")
 par(ask=FALSE)
}
