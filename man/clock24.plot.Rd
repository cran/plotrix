\name{clock24.plot}
\title{clock24.plot}
\usage{
 clock24.plot(lengths,clock.pos,...)
}
\alias{clock24.plot}
\arguments{
 \item{lengths}{numeric data vector. Magnitudes will be represented as
  line lengths.}
 \item{clock.pos}{numeric vector of positions on the 'clockface'.
  These must be in decimal hours and will be rescaled to radians.}
 \item{...}{additional arguments are passed to \code{radial.plot} and
  then to \code{plot}.}
}
\description{
 \code{clock24.plot} displays a plot of radial lines centered at
  the midpoint of the plot frame on a 24 hour 'clockface'. In contrast
  to \code{radial.plot}, the positions are interpreted as beginning
  at vertical (24:00) and moving clockwise.}
}
\value{nil}
\keyword{misc}
\authors{Jim Lemon}
\seealso{\code{polar.plot},\code{radial.plot}}
\examples{
 testlen<-c(rnorm(24)*2+5)
 testpos<-0:23+rnorm(24)/4
 clock24.plot(testlen,testpos,main="Test Clock24")
 # now do a 'daylight' plot
 clock24.plot(testlen[5:19],testpos[5:19],main="Test Clock24 daytime")
}
