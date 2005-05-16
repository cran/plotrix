\name{show.soil.texture}
\alias{show.soil.texture}
\title{show.soil.texture}
\usage{
 show.soil.texture(soiltexture,pch=NULL,col.symbols=NULL,bg.symbols=NA,
  show.legend=FALSE,...)
}
\arguments{
 \item{soiltexture}{Matrix of soil textures where each row is a 
  soil sample and three columns contain the proportions of the
  components sand, silt and clay in the range 0 to 1 or the
  percentages in the range 0 to 100.}
 \item{pch}{Symbols to use for plotting the observations.}
 \item{col.symbols}{Colors to use for plotting the observations.}
 \item{bg.symbols}{Fill colors for symbols.}
 \item{show.legend}{Logical - whether to show a legend.}
 \item{...}{additional arguments passed to \code{points}.}
}
\description{
 \code{show.soil.texture} displays points on the soil triangle plotted
  by \code{\link{soil.texture}} as symbols.}
}
\value{A list of the \code{x,y} positions of the soil types plotted.}
\author{Sander Oom and Jim Lemon}
\seealso{\code{\link{get.soil.texture}, \code{\link{soil.texture}}}}
\examples{
 data(oksoil)
 # first just show the soil triangle
 soil.texture()
 # now plot the observations
 show.soil.texture(oksoil,pch=2,col=2)
}
\keyword{misc}
