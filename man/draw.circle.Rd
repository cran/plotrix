\name{draw.circle}
\alias{draw.circle}
\title{Draw a circle.}
\description{Draws a circle on an existing plot.}
\usage{
 draw.circle(x,y,radius,nv=100,border=NULL,col=NA,lty=1,lwd=1)
}
\arguments{
 \item{x,y}{Coordinates of the center of the circle.}
 \item{radius}{Radius of the circle in user units.}
 \item{nv}{Number of vertices to draw the circle.}
 \item{border}{Color to use for drawing the circumference.}
 \item{col}{Color to use for filling the circle.}
 \item{lty}{Line type for the circumference.}
 \item{lwd}{Line width for the circumference.}
}
\value{
 A list with the x and y coordinates of the points on the circumference.
}
\note{
 The principal advantage of \samp{draw.circle} is that it adjusts for 
 the aspect ratio of the plot.
}
\author{Jim Lemon}
\seealso{\samp{\link{polygon}}}
\examples{
 plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="Test draw.circle")
 draw.circle(2,2,0.5,border="purple",lty=1,lwd=1)
 draw.circle(2.5,8,0.6,border="red",lty=3,lwd=3)
 draw.circle(4,3,0.7,border="green",lty=1,lwd=1)
 draw.circle(3.5,7,0.8,border="blue",lty=2,lwd=2)
}
\keyword{misc}
