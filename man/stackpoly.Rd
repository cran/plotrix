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
  xlim=NA,ylim=NA,lty=1,border=NA,col=NA,staxx=FALSE,axis4=TRUE,...)
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
 form a rising series of crests.
}
\author{Jim Lemon}
\seealso{\code{\link{polygon}}}
\examples{
 testx<-matrix(cumsum(abs(rnorm(100))),nrow=10)
 stackpoly(testx,main="Test Stackpoly",
  xaxlab=c("One","Two","Three","Four","Five",
  "Six","Seven","Eight","Nine","Ten"),border="black",staxx=TRUE)
}
\keyword{misc}
