\name{gradient.rect}
\title{Display a rectangle filled with an arbitrary color gradient.}
\usage{
 gradient.rect(xleft,ybottom,xright,ytop,reds,greens,blues,col=NULL,
  nslices=50,gradient="x")
}
\alias{gradient.rect}
\arguments{
 \item{xleft,ybottom,xright,ytop}{Positions of the relevant corners
  of the desired rectangle, as in \code{rect}.}
 \item{reds,greens,blues}{vectors of the values of the color components
  either as 0 to 1 or ,if any value is greater than 1, 0 to 255.}
 \item{col}{Vector of colors. If supplied, this takes precedence over
  \code{reds, greens, blues} and \code{nslices} will be set to its length.}
 \item{nslices}{The number of sub-rectangles that will be drawn.}
 \item{gradient}{whether the gradient should be horizontal (x) or vertical.}
}
\description{
 \code{gradient.rect} draws a rectangle consisting of \code{nslices}
  subrectangles of the colors in \code{col} or those returned by 
  \code{color.gradient} if \code{col} is NULL. The rectangle is
  'sliced' in the direction specified by \code{gradient}.}
}
\value{the vector of hexadecimal color values from \code{color.gradient} or
 \code{col}.}
\author{Jim Lemon}
\examples{
 # get an empty box
 plot(0:10,type="n",axes=FALSE)
 # run across the three primaries
 gradient.rect(1,0,3,6,reds=c(1,0),
  greens=c(seq(0,1,length=10),seq(1,0,length=10)),
  blues=c(0,1),gradient="y")
 # now a "danger gradient"
 gradient.rect(4,0,6,6,c(seq(0,1,length=10),rep(1,10)),
  c(rep(1,10),seq(1,0,length=10)),c(0,0),gradient="y")
 # now just send some colors
 gradient.rect(7,0,9,6,col=2:20,gradient="y")
}
\keyword{misc}
