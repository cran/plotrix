\name{boxed.labels}
\alias{boxed.labels}
\title{ Place labels in boxes }
\description{
 Places labels in boxes on an existing plot
}
\usage{boxed.labels(x,y,labels,col="white",border=TRUE,xpad=0.6,ypad=0.6,...)}
\arguments{
 \item{x,y}{x and y position of the centers of the labels}
 \item{col}{The color of the rectangles on which the labels are displayed.}
 \item{labels}{Text strings}
 \item{border}{Whether to draw borders around the rectangles.}
 \item{xpad,ypad}{Half the value of the proportion of the rectangles to the
  extent of the text within.}
 \item{...}{additional arguments passed to \code{text}.}
}
\details{
 This function is best for regularly spaced labels where overlapping is
 not a problem. See \code{thigmophobe.labels} for squeezing labels in tight
 places.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{spread.labels}, \code{thigmophobe.labels}}
\examples{
 x<-rnorm(10)
 y<-rnorm(10)
 plot(x,y,type="p")
 nums<-c("one","two","three","four","five","six","seven","eight","nine","ten")
 boxed.labels(x,y-0.1,nums)
}
\keyword{misc}
