\name{color.gradient}
\title{color.gradient}
\usage{
 color.gradient(reds,greens,blues,nslices=50,scale.up=FALSE)
}
\alias{color.gradient}
\arguments{
 \item{reds,greens,blues}{vectors of the values of the color components
  either as 0 to 1 or ,if any value is greater than 1, 0 to 255.}
 \item{nslices}{The number of color "slices".}
 \item{scale.up}{Whether to scale color values less than or equal to 1 into a
  0->255 range. If colors are specified in a 0->1 range, set this to TRUE.}
}
\description{
 \code{color.gradient} calculates sequences of three colors by linearly
  interpolating between the values specified for each color. If the length
  of a vector of colors is the same as \code{nslices} it is not changed.
  If any length is greater than \code{nslices}, it is truncated to that
  length. If any color values are less than 0 or greater than 255, they 
  are rescaled to that range regardless of the value of \code{scale.up}.}
}
\value{A vector of hexadecimal color values as used by \code{col}.}
\authors{Jim Lemon}
\seealso{\code{rgb.to.hex}}
\examples{
 # try it with red endpoints, green midpoints, and a blue vector longer
 # than 'nslices'
 color.gradient(c(0,1),c(1,0.6,0.4,0.3,0),seq(0.1,0.6,length=60),scale.up=TRUE) 
}
\keyword{misc}
