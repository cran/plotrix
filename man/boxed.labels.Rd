\name{boxed.labels}
\alias{boxed.labels}
\title{ Place labels in boxes }
\description{
 Places labels in boxes on an existing plot
}
\usage{
 boxed.labels(x,y=NA,labels,bg="white",border=TRUE,xpad=1.2,ypad=1.2,
  srt=0,cex=1,adj=0.5,...)
}
\arguments{
 \item{x,y}{x and y position of the centers of the labels. \samp{x} can be an
  \samp{\link{xy.coords}} list.}
 \item{bg}{The fill color of the rectangles on which the labels are displayed.}
 \item{labels}{Text strings}
 \item{border}{Whether to draw borders around the rectangles.}
 \item{xpad,ypad}{The proportion of the rectangles to the
  extent of the text within.}
 \item{srt}{Rotation of the labels. If 90 or 270 degrees, the box will be
  rotated 90 degrees.}
 \item{cex}{Character expansion. See \samp{text}.}
 \item{adj}{left/right adjustment. If this is set outside the function,
  the box will not be aligned properly.}
 \item{...}{additional arguments passed to \samp{text}.}
}
\details{
 The label(s) are displayed on a rectangular background. This may be useful
 for visibility. Only right angle rotations are allowed in \samp{boxed.labels}.
 Important change: \samp{xpad} and \samp{ypad} are now the full proportion of
 the box to text, not half. The user can now call \samp{cylindrect} or
 \samp{gradient.rect} for the background rectangle.
}
\note{
 This function is best for regularly spaced labels where overlapping is
 not a problem. See \samp{\link{thigmophobe.labels}} for placing labels 
 where overlap is likely.
}
\value{nil}
\author{Jim Lemon}
\seealso{\samp{\link{spread.labels}}, \samp{\link{thigmophobe.labels}}}
\examples{
 x<-rnorm(10)
 y<-rnorm(10)
 plot(x,y,type="p")
 nums<-c("one","two","three","four","five","six","seven","eight","nine","ten")
 boxed.labels(x,y-0.1,nums)
 # now label a barplot
 xpos<-barplot(c(1,3,2,4))
 boxed.labels(xpos,0.5,nums[1:4])
}
\keyword{misc}
