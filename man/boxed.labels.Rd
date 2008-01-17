\name{boxed.labels}
\alias{boxed.labels}
\title{ Place labels in boxes }
\description{
 Places labels in boxes on an existing plot
}
\usage{
 boxed.labels(x,y=NA,labels,bg="white",border=TRUE,xpad=0.6,ypad=0.6,
  srt=0,...)
}
\arguments{
 \item{x,y}{x and y position of the centers of the labels. \code{x} can be an
  \code{\link{xy.coords}} list.}
 \item{bg}{The fill color of the rectangles on which the labels are displayed.}
 \item{labels}{Text strings}
 \item{border}{Whether to draw borders around the rectangles.}
 \item{xpad,ypad}{Half the value of the proportion of the rectangles to the
  extent of the text within.}
 \item{srt}{Rotation of the labels. If 90 or 270 degrees, the box will be
  rotated 90 degrees.}
 \item{...}{additional arguments passed to \code{text}.}
}
\note{
 This function is best for regularly spaced labels where overlapping is
 not a problem. See \code{\link{thigmophobe.labels}} for placing labels 
 where overlap is likely.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{spread.labels}}, \code{\link{thigmophobe.labels}}}
\examples{
 x<-rnorm(10)
 y<-rnorm(10)
 plot(x,y,type="p")
 nums<-c("one","two","three","four","five","six","seven","eight","nine","ten")
 boxed.labels(x,y-0.1,nums)
 readline("Press <Enter> to continue")
 # now label a barplot
 xpos<-barplot(c(1,3,2,4))
 boxed.labels(xpos,0.5,nums[1:4])
}
\keyword{misc}
