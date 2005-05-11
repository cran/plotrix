\name{spread.labels}
\alias{spread.labels}
\title{ Spread labels for irregularly spaced values }
\description{
 Places labels for irregularly spaced values in a regular staggered order
}
\usage{
 spread.labels(x,y,labels=NULL,offset,col="white",border=FALSE,...)
}
\arguments{
 \item{x,y}{x and y data values}
 \item{labels}{text strings}
 \item{offset}{How far away from the data points to place the labels.}
 \item{col}{The color of the background rectangles on which to place the
  labels.}
 \item{border}{Whether to draw a border around the labels.}
 \item{...}{additional arguments passed to \code{boxed.labels}.}
}
\details{
 This function is mainly useful when labeling irregularly spaced data points
 that are "spread out" along one dimension. It places the labels regularly
 spaced and staggered on the long dimension of the data, drawing lines from
 each label to the point it describes.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{boxed.labels}}
\examples{
 x<-rnorm(10)/10
 y<-sort(rnorm(10))
 plot(x,y,xlim=c(-1,1),type="p")
 nums<-c("one","two","three","four","five","six","seven","eight","nine","ten")
 spread.labels(x,y,nums,0.5)
}
\keyword{misc}
