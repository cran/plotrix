\name{rgb.to.hex}
\title{rgb.to.hex}
\usage{
 rgb.to.hex(rgb,scale.up=TRUE)
}
\alias{rgb.to.hex}
\arguments{
 \item{rgb}{A numeric vector of length at least 3 representing the relative
  red, green and blue values. Each value must be between 0 and 255.}
 \item{scale.up}{A logical value indicating whether to scale up when all
  values are between 0 and 1.}
}
\description{
 \code{rgb.to.hex} converts the first three values in \code{rgb} to a six
 digit hexadecimal string preceded by a hash character. If all three values 
 are between 0 and 1, all are scaled up to a range of 0-255 unless 
 \code{scale.up} is FALSE.}
}
\value{hexadecimal color}
\authors{Jim Lemon}
\seealso{\code{gradient.rect}}
\examples{
 # a light purple color in a 0-1 format
 rgb.to.hex(c(0.9,0.5,0.9))
 # supressing the scaling gives almost pure black
 rgb.to.hex(c(0.9,0.5,0.9),FALSE)
}
\keyword{misc}
