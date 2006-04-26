\name{triax.frame}
\alias{triax.frame}
\title{Triangle plot}
\description{
 Display a three axis frame with optional grid.
}
\usage{
 triax.frame(main="",
 at=list(l=seq(0.1,0.9,by=0.1),r=seq(0.1,0.9,by=0.1),b=seq(0.1,0.9,by=0.1)),
 axis.labels=NULL,tick.labels=NULL, col.axis="black",
 show.grid=FALSE,col.grid="gray",lty.grid=par("lty"))
}
\arguments{
 \item{main}{The title of the triangle plot. Defaults to nothing.}
 \item{at}{A list with three components, "l", "r" and "b", giving the 
  left, right and bottom tick positions on the three axes.}
 \item{axis.labels}{Labels for the three axes. Defaults to the column names.}
 \item{tick.labels}{Tick labels - a list with three components, "l",
  "r" and "b", giving the left, right and bottom tick labels.
  Defaults to argument \code{at} (proportions).}
 \item{col.axis}{Color of the triangular axes, ticks and labels.}
 \item{show.grid}{Whether to display grid lines at the ticks.}
 \item{col.grid}{Color of the grid lines. Defaults to gray.}
 \item{lty.grid}{Type of line for the grid.}
}
\details{
 \code{triax.frame} displays a triangular plot area on which proportions
  or percentages may be displayed. An optional grid may also be displayed.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{triax.points}},\code{\link{triax.abline}}}
\examples{
 triax.frame(main="DEFAULT")
}
\keyword{misc}
