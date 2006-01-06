\name{thigmophobe.points}
\alias{thigmophobe.points}
\title{Shift close points away from each other.}
\usage{
 thigmophobe.points(x,y,away=NULL,tol=NULL)
}
\arguments{
 \item{x,y}{Numeric data vectors or the first two columns of a matrix
  or data frame. Typically the x/y coordinates of points to be plotted.}
 \item{away}{How far to move overlying points in user units. Defaults to
  1/4 the width of a lower case "o" in the x direction and 1/4 of the
  height of a lower case "o" in the y direction. Because both points are
  moved, the actual separation will be twice this distance.}
 \item{tol}{The largest distance between points that will be considered
  to be overlying. Defaults to 1/2 of the width of a lower case "o" in 
  the x direction and 1/2 of the height of a lower case "o" in the y 
  direction.}
}
\description{
 \code{thigmophobe.points} checks for x,y points that are closer than
 \code{tol}. Such pairs are moved apart in the dimension which they are
  closest. Note that clusters of more than two points may not be handled 
  optimally.
}
\value{
 A list with two components. For x-y pairs farther apart than \code{tol},
 the elements will be the same as in the original.
}
\details{
 \code{thigmophobe.points} is most useful where a small number of pairs
  of points are very close together. When larger clusters occur, use
  \code{cluster.overplot} or \code{count.overplot}.
}
\keyword{misc}
\author{Jim Lemon}
\seealso{\code{\link{count.overplot}},\code{\link{sizeplot}},
 \code{\link{cluster.overplot}}}
\examples{
 xy.mat<-cbind(rnorm(80),rnorm(80))
 plot(xy.mat,main="Before calling thigmophobe",xlab="X",ylab="Y")
 if(dev.interactive()) par(ask=TRUE)
 plot(thigmophobe.points(xy.mat),main="After calling thigmophobe",
  xlab="X",ylab="Y")
 par(ask=FALSE)
}
