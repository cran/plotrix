\name{show.soil.texture}
\alias{show.soil.texture}
\title{Display points on a soil texture triangle.}
\usage{
 show.soil.texture(soiltexture,pch=NULL,col.symbols=NULL,bg.symbols=NA,
  show.legend=FALSE)
}
\arguments{
 \item{soiltexture}{Matrix of soil textures where each row is a 
  soil sample and three columns contain the proportions of the
  components sand, silt and clay in the range 0 to 1 or the
  percentages in the range 0 to 100.}
 \item{pch}{Symbols to use for plotting the observations.}
 \item{col.symbols}{Colors to use for plotting the observations.}
 \item{bg.symbols}{Fill colors for symbols.}
 \item{show.legend}{Logical - whether to show a legend. If there are more 
  than about six soil textures, it is probably better to add a legend 
  separately.}
}
\description{
 \code{show.soil.texture} displays points on the soil triangle plotted
  by \code{\link{soil.texture}} as symbols. It is most useful for adding 
  new points to an existing plot.}
}
\value{A list of the \code{x,y} positions of the soil types plotted.}
\author{Sander Oom and Jim Lemon}
\seealso{\code{\link{get.soil.texture}}, \code{\link{soil.texture}}}
\examples{
 data(soils)
 # first just show the soil triangle
 soil.texture(main="Soil Texture Triangle")
 # now plot the observations
 show.soil.texture(soils,pch=2,col.symbols=2)
}
\keyword{misc}
