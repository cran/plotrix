\name{color.scale}
\alias{color.scale}
\title{Turn values into colors.}
\description{Transform numeric values into colors.}
\usage{
 color.scale(x,redrange=c(0,1),greenrange=c(0,1),bluerange=c(0,1),
  extremes=NA,na.color=NA)
}
\arguments{
 \item{x}{a numeric vector, matrix or data frame}
 \item{redrange,greenrange,bluerange}{color ranges into which 
  to scale \samp{x}}
 \item{extremes}{The colors for the extreme values of \samp{x}.}
 \item{na.color}{The color to use for NA values of \samp{x}.}
}
\details{
 \samp{color.scale} calculates a sequence of colors by a linear
  transformation of the numeric values supplied into the ranges 
  for red, green and blue. If only one number is supplied for a
  color range, that color remains constant for all values of \samp{x}.
  If more than two values are supplied, the \samp{x} values will be
  split into equal ranges (one less than the number of colors) and 
  the transformation carried out on each range. Values for a color
  range must be between 0 and 1.
  
  If \samp{extremes} is not NA, the ranges will be calculated from
  its values using \samp{col2rgb}, even if ranges are also supplied.
  \samp{extremes} allows the user to just pass the extreme color values
  in any format that \samp{col2rgb} will accept.
  
  The user may not want the color scheme to be continuous across some
  critical point, often zero. In this case, color scale can be called
  separately for the values below and above zero. See the second example
  for \samp{color2D.matplot}.
}
\note{
 The function is useful for highlighting a numeric dimension or adding
 an extra "dimension" to a plot.
}
\value{A vector or matrix of hexadecimal color values.}
\author{Jim Lemon}
\seealso{\samp{\link{rescale}}, \samp{\link{col2rgb}}}
\examples{
 # go from green through yellow to red with no blue
 x<-rnorm(20)
 y<-rnorm(20)
 # use y for the color scale
 plot(x,y,col=color.scale(y,c(0,1,1),c(1,1,0),0),main="Color scale plot",
  pch=16,cex=2)
}
\keyword{misc}
