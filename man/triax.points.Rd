\name{triax.points}
\alias{triax.points}
\title{Triangle plot points}
\description{
 Display points on a triangle plot.
}
\usage{
 triax.points(x,show.legend=FALSE,label.points=FALSE,point.labels=NULL,
 col.symbols=par("fg"),pch=par("pch"),bg.symbols=par("bg"),...)
}
\arguments{
 \item{x}{Matrix or data frame where each row is three proportions or 
  percentages that must sum to 1 or 100 respectively.}
 \item{show.legend}{Logical - whether to display a legend.}
 \item{label.points}{Logical - whether to call \code{thigmophobe.labels}
  to label the points.}
 \item{point.labels}{Optional labels for the points and/or legend.}
 \item{col.symbols}{Color of the symbols representing each value.}
 \item{pch}{Symbols to use in plotting values.}
 \item{bg.symbols}{Background color for plotting symbols.}
 \item{...}{Additional arguments passed to \code{points}.}
}
\details{
 In order for \code{triax.points} to add points to an existing plot,
 the argument \code{no.add} in the initial call to \code{triax.plot} 
 must be set to \code{FALSE}. It is then up to the user to call \code{par}
 as in the example below to restore plotting parameters altered during
 the triangle plot.
 
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
 triax.return<-triax.plot(soils[1:10,],
  main="Adding points to a triangle plot",no.add=FALSE)
 if(dev.interactive()) par(ask=TRUE)
 triax.points(soils[11:20,],col.symbols="green",pch=3)
 par(triax.return$oldpar)
 if(dev.interactive()) par(ask=FALSE)
}
\keyword{misc}
