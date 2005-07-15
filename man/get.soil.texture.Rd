\name{get.soil.texture}
\alias{get.soil.texture}
\title{Enter soil texture data.}
\usage{
 get.soil.texture(use.percentages=FALSE)
}
\arguments{
 \item{use.percentages}{Logical - whether to treat the entries as
  percentages and scale to proportions.}
}
\description{
 \code{get.soil.texture} allows the user to enter soil textures as the
  proportions or percentages of three components, sand, silt and clay.}
}
\value{A matrix of the components of one or more soil samples.}
\author{Sander Oom and Jim Lemon}
\seealso{\code{\link{show.soil.texture}, \code{\link{soil.texture}}}}
\examples{
 newsp<-get.soil.texture()
 # show the soil triangle
 get.soil.texture()
 # now plot the observations
 show.soil.texture(newsp)
}
\keyword{misc}
