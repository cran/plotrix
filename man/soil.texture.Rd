\name{soil.texture}
\alias{soil.texture}
\title{Soil texture triangle plot}
\description{
 Display a soil texture triangle with optional grid, labels and soil texture
 points.
}
\usage{
 soil.texture(soiltexture=NULL,
 show.names=TRUE,show.lines=TRUE,show.grid=FALSE,bg.names="white",main="",
 col.axis="black",col.names="gray",col.lines="gray",col.grid="gray",...)
}
\arguments{
 \item{soiltexture}{Matrix of soil textures where each row is a 
  soil sample and three columns contain the proportions of the
  components sand, silt and clay in the range 0 to 1 or 
  percentages in the range 0 to 100.}
 \item{show.names}{Logical - whether to show the name of different
  soil types within the soil triangle.}
 \item{show.lines}{Logical - whether to show the boundaries of the
  different soil types within the soil triangle.}
 \item{show.grid}{Logical - whether to show grid lines at each 10%
  level of each soil component.}
 \item{bg.names}{Color to use when drawing a blank patch for the names
  of soil types. Defaults to white but the user may wish to use the
  plot background color.}
 \item{main}{The title of the soil texture plot. Defaults to none.}
 \item{col.axis}{Color of the triangular axes, ticks and labels.}
 \item{col.names}{Color of the soil names. Defaults to gray.}
 \item{col.lines}{Color of the boundary lines. Defaults to gray.}
 \item{col.grid}{Color of the grid lines. Defaults to gray.}
 \item{...}{Additional arguments passed to \code{\link{show.soil.texture}}
  or \code{points}.}
}
\details{
 \code{soil.texture} displays a triangular plot area on which soil
  textures defined as proportions of sand, silt and clay can be plotted.
  Optional grid, vertex labels, soil type divisions and names may also 
  be displayed.
  If a matrix of soil textures is present, this will be passed to
  \code{\link{show.soil.texture}} and the textures will be plotted.}
}
\value{If \code{soiltexture} was included, a list of the \code{x,y} 
 positions of the soil types plotted. If not, nil.}
\author{Sander Oom and Jim Lemon}
\seealso{\code{\link{get.soil.texture}}, \code{\link{show.soil.texture}}}
\examples{
 data(soils)
 soil.texture()
 if(dev.interactive()) par(ask=TRUE)
 main.title<-"DEFAULT"
 soil.texture(soils,main=main.title,pch=2)
 main.title<-"LINES AND NAMES"
 soil.texture(soils,main=main.title,show.lines=TRUE,show.names=TRUE,
  pch=3)
 main.title<- "GRID AND LEGEND"
 soil.texture(soils[1:6,],main=main.title,show.grid=TRUE,pch=4,
  col.symbols=1:6,show.legend=TRUE)
 if(dev.interactive()) par(ask=FALSE)
}
\keyword{misc}
