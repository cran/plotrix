\name{triax.frame}
\alias{triax.frame}
\title{Triangle plot frame}
\description{
 Display a three axis frame with optional grid.
}
\usage{
 triax.frame(main="",at=seq(0.1,0.9,by=0.1),
 axis.labels=NULL,tick.labels=NULL,col.axis="black",cex.axis=1,cex.ticks=1,
 align.labels=TRUE,show.grid=FALSE,col.grid="gray",lty.grid=par("lty"))
}
\arguments{
 \item{main}{The title of the triangle plot. Defaults to nothing.}
 \item{at}{The tick positions on the three axes.}
 \item{axis.labels}{Labels for the three axes in the order bottom, right
  left. Defaults to the column names.}
 \item{tick.labels}{The tick labels for the axes.
  Defaults to argument \code{at} (proportions).}
 \item{col.axis}{Color of the triangular axes, ticks and labels.}
 \item{cex.axis}{Character expansion for axis labels.}
 \item{cex.ticks}{Character expansion for the tick labels.}
 \item{align.labels}{Logical - whether to align axis and tick labels with
  the axes.}
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
 par(xpd=TRUE)
 triax.frame(main="DEFAULT")
 par(xpd=FALSE)
}
\keyword{misc}
