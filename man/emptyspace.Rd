\name{emptyspace}
\alias{emptyspace}
\title{ Find an empty space on a plot. }
\description{
 Try to find the largest empty rectangle on a plot.
}
\usage{
 emptyspace(x,y=NA,bars=FALSE)
}
\arguments{
 \item{x,y}{x and y positions of the points or centers and heights
  of the bars}
 \item{bars}{Whether to add x and y points to represent the bars if the
  plot is a barplot.}
}
\details{
 \code{emptyspace} divides the area defined by \code{par("usr")}
 into smaller and smaller rectangles until at least one rectangle has no
 points defined by \code{x} and \code{y} within it. It then tries to find
 the largest such rectangle if more than one exists and calculates its
 center. If the plot is very crowded, the resulting rectangle may be very
 small.
 
 \code{emptyspace} will accept a list of at least two matrices as if it
 was returned from \code{brkdn.plot}, calculate the positions of the
 ends of the dispersion bars and then try to find an empty rectangle.
 It will also accept a list of x-y coordinates, looking for the first
 element to have the name \code{x}, and set \code{y} to the second element.
 
 Note that if there are any NAs in \code{x} or \code{y}, emptyspace will fail.
}
\value{
 The \code{x} and \code{y} coordinates of the center of the rectangle
 found.
}
\author{Jim Lemon}
\examples{
 x<-rnorm(10)
 y<-rnorm(10)
 plot(x,y,main="Find the empty space",xlab="X",ylab="Y")
 es<-emptyspace(x,y)
 boxed.labels(es,labels="Here is the\nempty space")
}
\keyword{misc}
