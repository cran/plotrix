\name{color2D.matplot}
\alias{color2D.matplot}
\title{Display a numeric matrix as color matrix}
\description{
 Display the values of a numeric 2D matrix or data frame as colored rectangles.
}
\usage{
 color2D.matplot(x,redrange=c(0,1),greenrange=c(0,1),bluerange=c(0,1),
  show.legend=FALSE,...)
}
\arguments{
 \item{x}{data values}
 \item{redrange, greenrange, bluerange}{the ranges of red, green and blue
  that will be scaled to represent the range of numeric values}
 \item{show.legend}{whether to display a color range rectangle with the
  extreme numeric values in the lower left corner of the plot}
 \item{...}{arguments passed to \code{plot}.}
}
\value{nil}
\details{
 Displays a plot with the same number of rectangular cells as there are 
 numeric values in the matrix or data frame. Each rectangle is colored to
 represent its corresponding value. The rectangles are arranged in the
 conventional display of a 2D matrix with rows beginning at the top and
 columns at the left. The color scale defaults to black for the minimum
 value and white for the maximum.
}
\author{Jim Lemon}
\seealso{\code{\link{color.scale}}}
\examples{
 x<-matrix(rnorm(1024)+sin(seq(0,2*pi,length=1024)),nrow=32)
 color2D.matplot(x,c(1,0),c(0,0),c(0,1),show.legend=TRUE,
  xlab="Columns",ylab="Rows",main="2D matrix plot")
}
\keyword{misc}
