\name{triax.points}
\alias{triax.points}
\title{Triangle plot}
\description{
 Display a triangle plot with optional grid.
}
\usage{
 triax.points(x,show.legend=FALSE,label.points=FALSE,point.labels=NULL,
 col.symbols=par("fg"),pch=par("pch"),bg.symbols=par("bg"),...)
}
\arguments{
 \item{x}{Matrix or data frame where each row is three proportions or 
  percentages that must sum to 1 or 100 respectively.}
 \item{show.legend}{Logical - whether to display a legend.}
 \item{label.points}{Logical - whether to call /code{thigmophobe.labels} to
  label the points.}
 \item{point.labels}{Optional labels for the points and/or legend.}
 \item{col.symbols}{Color of the symbols representing each value.}
 \item{pch}{Symbols to use in plotting values.}
 \item{bg.symbols}{Background color for plotting symbols.}
 \item{...}{Additional arguments passed to \code{points}.}
}
\details{
 \code{triax.points} displays each triplet of proportions or percentages
  as a symbol on the triangle plot. Unless each triplet sums to 1 
  (or 100), they will not plot properly and \code{triax.points} will 
  complain appropriately.
}
\value{A list of the \code{x,y} positions plotted.}
\author{Jim Lemon}
\seealso{\code{\link{triax.plot}},\code{\link{thigmophobe.labels}}}
\examples{
 data(soils)
 triax.plot(soils[1:10,],main="Adding points to a triangle plot")
 if(dev.interactive()) par(ask=TRUE)
 triax.points(soils[11:20,],col.symbols="green",pch=3)
 if(dev.interactive()) par(ask=FALSE)
}
\keyword{misc}
