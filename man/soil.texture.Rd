\name{soil.texture}
\alias{soil.texture}
\title{Soil texture triangle plot}
\description{
 Display a soil texture triangle with optional grid, labels and soil texture
 points.
}
\usage{
 soil.texture(soiltexture=NULL,main="",at=seq(0.1,0.9,by=0.1),
 axis.labels=c("percent sand","percent silt","percent clay"),
 tick.labels=list(l=seq(10,90,by=10),r=seq(10,90,by=10),b=seq(10,90,by=10)),
 show.names=TRUE,show.lines=TRUE,col.names="gray",bg.names=par("bg"),
 show.grid=FALSE,col.axis="black",col.lines="gray",col.grid="gray",
 lty.grid=3,show.legend=FALSE,label.points=FALSE,point.labels=NULL,
 col.symbols="black",pch=par("pch"),...)
}
\arguments{
 \item{soiltexture}{Matrix of soil textures where each row is a 
  soil sample and three columns contain the proportions of the
  components sand, silt and clay in the range 0 to 1 or 
  percentages in the range 0 to 100.}
 \item{main}{The title of the soil texture plot. Defaults to nothing.}
 \item{at}{Positions on the three axes where ticks will be drawn.}
 \item{axis.labels}{Labels for the axes.}
 \item{tick.labels}{The tick labels for the three axes.}
 \item{show.names}{Logical - whether to show the names of different
  soil types within the soil triangle.}
 \item{show.lines}{Logical - whether to show the boundaries of the
  different soil types within the soil triangle.}
 \item{col.names}{Color of the soil names. Defaults to gray.}
 \item{bg.names}{Color to use when drawing a blank patch for the names
  of soil types.}
 \item{show.grid}{Logical - whether to show grid lines at each 10%
  level of each soil component.}
 \item{col.axis}{Color of the triangular axes, ticks and labels.}
 \item{col.lines}{Color of the boundary lines. Defaults to gray.}
 \item{col.grid}{Color of the grid lines. Defaults to gray.}
 \item{lty.grid}{Type of line for the grid. Defaults to dashed.}
 \item{show.legend}{Logical - whether to display a legend.}
 \item{label.points}{Logical - whether to call \code{thigmophobe.labels} to
  label the points.}
 \item{point.labels}{Optional labels for the points or legend.}
 \item{col.symbols}{Color of the symbols representing each value.}
 \item{pch}{Symbols to use in plotting values.}
 \item{...}{Additional arguments passed to \code{\link{triax.points}}
  and then \code{points}.}
}
\details{
 \code{soil.texture} displays a triangular plot area on which soil
  textures defined as proportions of sand, silt and clay can be plotted.
  Optional grid, vertex labels, soil type divisions and names may also 
  be displayed. If a matrix of soil textures is present, these will be
  plotted.
}
\value{If \code{soiltexture} was included, a list of the \code{x,y} 
 positions of the soil types plotted. If not, nil.
}
\note{This is now a special case of \code{triax.plot}.}
\author{Sander Oom and Jim Lemon}
\seealso{\code{\link{get.soil.texture}}, \code{\link{triax.plot}}}
\examples{
 data(soils)
 soil.texture(main="NO DATA")
 if(dev.interactive()) par(ask=TRUE)
 soil.texture(soils,main="DEFAULT",pch=2)
 soil.texture(soils,main="LINES AND NAMES",show.lines=TRUE,
  show.names=TRUE,pch=3)
 soil.texture(soils[1:6,],main="GRID AND LEGEND",show.grid=TRUE,pch=4,
  col.symbols=1:6,show.legend=TRUE)
 if(dev.interactive()) par(ask=FALSE)
}
\keyword{misc}
