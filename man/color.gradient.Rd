\name{color.gradient}
\title{color.gradient}
\usage{
 color.gradient(reds,greens,blues,nslices=50)
}
\alias{color.gradient}
\arguments{
 \item{reds,greens,blues}{vectors of the values of the color components
  as 0 to 1.}
 \item{nslices}{The number of color "slices".}
}
\description{
 \code{color.gradient} calculates sequences of colors by linearly
  interpolating between the values specified for each color. If the length
  of a vector of colors is the same as \code{nslices} it is not changed.
  If any length is greater than \code{nslices}, it is truncated to that
  length. If any color values are less than 0 or greater than 1, they 
  are rescaled to that range.
  The function is mainly useful for defining a set of colors to represent
  a known number of gradations. Such a set can be used to assign a grade
  to values (e.g. points on a scatterplot) and display a color bar using
  \code{gradient.rect} as a legend.}
}
\value{A vector of hexadecimal color values as used by \code{col}.}
\author{Jim Lemon}
\seealso{\code{\link{rescale}, \link{color.scale}}}
\examples{
 # try it with red endpoints, green midpoints, and a blue vector longer
 # than 'nslices'
 color.gradient(c(0,1),c(1,0.6,0.4,0.3,0),seq(0.1,0.6,length=60))
}
\keyword{misc}
