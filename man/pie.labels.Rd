\name{pie.labels}
\alias{pie.labels}
\title{ Place labels on a pie chart }
\description{
 Places labels on a pie chart
}
\usage{
 pie.labels(x,y,angles,labels,radius=1,col="white",border=TRUE,...)
}
\arguments{
 \item{x,y}{x and y position of the center of the pie chart}
 \item{angles}{A numeric vector representing angles in radians. This is
  the return value of \code{floating.pie}.}
 \item{labels}{Text strings to label each sector.}
 \item{radius}{The radius at which to place the labels in user units. The
  default is 1.}
 \item{col}{The color of the rectangles on which the labels are displayed.}
 \item{border}{Whether to draw borders around the rectangles.}
 \item{...}{Arguments passed to \code{boxed.labels}.}
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{floating.pie}, \code{boxed.labels}}
\examples{
 pieval<-c(2,4,6,8)
 plot(1:5,type="n")
 bisect.angles<-floating.pie(3,3,pieval)
 pie.labels(3,3,bisect.angles,c("two","four","six","eight"))
}
\keyword{misc}
