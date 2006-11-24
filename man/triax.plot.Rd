\name{triax.plot}
\alias{triax.plot}
\title{Triangle plot}
\description{
 Display a triangle plot with optional grid.
}
\usage{
 triax.plot(x=NULL,main="",at=seq(0.1,0.9,by=0.1),
 axis.labels=NULL,tick.labels=NULL,col.axis="black",cex.axis=1,cex.ticks=1,
 align.labels=TRUE,show.grid=FALSE,col.grid="gray",lty.grid=par("lty"),
 show.legend=FALSE,label.points=FALSE,point.labels=NULL,
 col.symbols="black",pch=par("pch"),...)
}
\arguments{
 \item{x}{Matrix where each row is three proportions or percentages
  that must sum to 1 or 100 respectively.}
 \item{main}{The title of the soil texture plot. Defaults to nothing.}
 \item{at}{The tick positions on the three axes.}
 \item{axis.labels}{Labels for the three axes in the order left, right,
  bottom. Defaults to the column names.}
 \item{tick.labels}{The tick labels for the three axes as a list with
  three components l, r and b (left, right and bottom).
  Defaults to argument \code{at} (proportions).}
 \item{col.axis}{Color of the triangular axes, ticks and labels.}
 \item{cex.axis}{Character expansion for axis labels.}
 \item{cex.ticks}{Character expansion for the tick labels.}
 \item{align.labels}{Logical - whether to align axis and tick labels with
  the axes.}
 \item{show.grid}{Whether to display grid lines at the ticks.}
 \item{col.grid}{Color of the grid lines. Defaults to gray.}
 \item{lty.grid}{Type of line for the grid.}
 \item{show.legend}{Logical - whether to display a legend.}
 \item{label.points}{Logical - whether to call \code{thigmophobe.labels} to
  label the points.}
 \item{point.labels}{Optional labels for the points and/or legend.}
 \item{col.symbols}{Color of the symbols representing each value.}
 \item{pch}{Symbols to use in plotting values.}
 \item{...}{Additional arguments passed to \code{points}.}
}
\details{
 \code{triax.plot} displays a triangular plot area on which proportions
  or percentages are displayed. An optional grid may also be displayed.
}
\note{
 A three axis plot can only properly display one or more
 sets of three proportions that each sum to 1 (or percentages that sum
 to 100). Other values may be scaled to proportions (or percentages), 
 but unless each set of three sums to 1 (or 100), they will not plot
 properly and \code{triax.plot} will complain appropriately. Note also
 that \code{triax.plot} will only display properly in a square plot,
 which is forced by \code{par(pty="s")}.
}
\value{A list of the \code{x,y} positions plotted.}
\author{Jim Lemon}
\seealso{\code{\link{triax.points}},\code{\link{triax.abline}},
 \code{\link{thigmophobe.labels}}}
\examples{
 data(soils)
 triax.plot(soils[1:10,],main="DEFAULT")
 if(dev.interactive()) par(ask=TRUE)
 triax.plot(soils[1:10,],main="PERCENTAGES",
  tick.labels=seq(10,90,by=10),pch=3)
 main.title<- 
 triax.plot(soils[1:6,],main="GRID AND LEGEND",show.grid=TRUE,
  show.legend=TRUE,col.symbols=1:6,pch=4)
 if(dev.interactive()) par(ask=FALSE)
}
\keyword{misc}
