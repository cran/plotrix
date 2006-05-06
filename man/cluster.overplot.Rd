\name{cluster.overplot}
\alias{cluster.overplot}
\title{Shift overlying points into clusters.}
\usage{
 cluster.overplot(x,y,away=NULL,tol=NULL)
}
\arguments{
 \item{x,y}{Numeric data vectors or the first two columns of a matrix
  or data frame. Typically the x/y coordinates of points to be plotted.}
 \item{away}{How far to move overlying points in user units. Defaults to
  the width of a lower case "o" in the x direction and 5/8 of the
  height of a lower case "o" in the y direction.}
 \item{tol}{The largest distance between points that will be considered
  to be overlying. Defaults to 1/2 of the width of a lower case "o" in 
  the x direction and 1/2 of the height of a lower case "o" in the y 
  direction.}
}
\description{
 \code{cluster.overplot} checks for overlying points in the x and y
  coordinates passed. Those points that are overlying are moved to form
  a small cluster of up to nine points. For large numbers of overlying
  points, see \code{\link{count.overplot}} or \code{\link{sizeplot}}.}
}
\value{
 A list with two components. For unique x-y pairs the elements will be 
 the same as in the original. For overlying points up to eight additional 
 points will be generated that will create a cluster of points instead of one.
}
\keyword{misc}
\author{Jim Lemon}
\seealso{\code{\link{count.overplot}},\code{\link{sizeplot}}}
\examples{
 xy.mat<-cbind(sample(1:10,200,TRUE),sample(1:10,200,TRUE))
 plot(cluster.overplot(xy.mat),main="Cluster overplot test")
}
