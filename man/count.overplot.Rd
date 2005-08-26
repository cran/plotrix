\name{count.overplot}
\alias{count.overplot}
\title{Show overlying points as counts}
\usage{
 count.overplot(x,y,tol=NULL,...)
}
\arguments{
 \item{x,y}{Two numeric data vectors or the first two columns of a matrix
  or data frame. Typically the x/y coordinates of points to be plotted.}
 \item{tol}{The largest distance between points that will be considered
  to be overlying.}
 \item{...}{additional arguments passed to \code{plot}.}
}
\description{
 \code{count.overplot} checks for overlying points defined as points
  separated by a maximum of \code{tol}, a two element numeric vector of
  the x and y tolerance. This defaults to 1/1000 of the plot range.
}
\value{nil}
\keyword{misc}
\author{Jim Lemon}
\seealso{\code{\link{cluster.overplot}},\code{\link{sizeplot}}}
\examples{
 xy.mat<-cbind(sample(1:10,200,TRUE),sample(1:10,200,TRUE))
 count.overplot(xy.mat)
}
