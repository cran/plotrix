\name{stackpoly}
\alias{stackpoly}
\title{
 Display the columns of a matrix or data frame as stacked polygons.
}
\description{
 Plot one or more columns of numeric values as the top edges of polygons
 instead of lines.
}
\usage{
 stackpoly(x,y=NULL,main="",xlab="",ylab="",xat=NA,xaxlab=NA,
  xlim=NA,ylim=NA,lty=1,border=NA,col=NA,staxx=FALSE,stack=FALSE,
  axis4=TRUE,...)
}
\arguments{
 \item{x}{A numeric data frame or matrix with the \code{x} values. If
  \code{y} is NULL, these will become the \code{y} values and the \code{x}
  positions will be the integers from 1 to dim{x}[1].}
 \item{y}{The \code{y} values.}
 \item{main}{The title for the plot.}
 \item{xlab,ylab}{x and y axis labels for the plot.}
 \item{xat}{Where to put the optional xaxlabs.}
 \item{xaxlab}{Optional labels for the x positions.}
 \item{xlim}{Optional x limits.}
 \item{ylim}{Optional y limits.}
 \item{lty}{Line type for the polygon borders.}
 \item{border}{Color for the polygon borders.}
 \item{col}{Color to fill the polygons.}
 \item{staxx}{Whether to call \code{staxlab} to stagger the x axis labels.}
 \item{stack}{Whether to stack the successive values on top of each other.}
 \item{axis4}{Whether to display the right ordinate on the plot.}
 \item{...}{Additional arguments passed to \code{plot}.}
}
\value{
 nil
}
\details{
 \code{stackpoly} is similar to a line plot with the area under the
 lines filled with color(s). Ideally, each successive set of y values
 is greater than the values in the previous set so that the polygons 
 form a rising series of crests. If \code{stack} is TRUE, this is not a
 problem unless some values of \code{x} are negative.
 
 If \code{x} or \code{y} is a vector, not a matrix or list, the values will
 be displayed as a "waterfall plot".
}
\author{Jim Lemon and Thomas Petzoldt (waterfall plot option)}
\seealso{\code{\link{polygon}}}
\examples{
 testx<-matrix(abs(rnorm(100)),nrow=10)
 stackpoly(matrix(cumsum(testx),nrow=10),main="Test Stackpoly I",
  xaxlab=c("One","Two","Three","Four","Five",
  "Six","Seven","Eight","Nine","Ten"),border="black",staxx=TRUE)
 stackpoly(testx,main="Test Stackpoly II",
  xaxlab=c("One","Two","Three","Four","Five",
  "Six","Seven","Eight","Nine","Ten"),border="black",
  staxx=TRUE,stack=TRUE)
 stackpoly(rev(sort(testx-mean(testx))),main="Test Waterfall Plot",
  col="green",border="black")
}
\keyword{misc}
