\name{draw.arc}
\alias{draw.arc}
\title{ Draw arc }
\description{
  Draw one or more arcs using classic graphics.
}
\usage{
 draw.arc(x=1,y=NULL,radius=1,angle1=deg1*pi/180,angle2=deg2*pi/180, 
  deg1=0,deg2=45,n=35,col=1,...)
}
\arguments{
  \item{x}{ x coordinate of center.  Scalar or vector. }
  \item{y}{ y coordinate of center.  Scalar or vector. }
  \item{radius}{ radius.  Scalar or vector.  }
  \item{angle1}{ Starting angle in radians. Scalar or vector. }
  \item{angle2}{ Ending angle in radians. Scalar or vector. }
  \item{deg1}{ Starting angle in degrees. Scalar or vector. }
  \item{deg2}{ Ending angle in degrees. Scalar or vector. }
  \item{n}{ Number of polygons to use to approximate the arc. }
  \item{col}{ Arc colors. }
  \item{...}{ Other arguments passed to segments.  Vectorization 
   is not supported for these. }
}
\details{
  Draws one or more arcs from \code{angle1} to \code{angle2}.
  If \code{angle1} is numerically greater than \code{angle2},
  then the angles are swapped.

  Be sure to use an aspect ratio of 1 as shown in the example to avoid
  distortion.
}
\value{
  Returns a matrix of expanded arguments invisibly.
}
\author{Gabor Grothendieck}
\examples{

   plot(1:10, asp = 1,main="Test draw.arc")
   draw.arc(5, 5, 1:10/10, deg2 = 1:10*10, col = "blue")
   draw.arc(8, 8, 1:10/10, deg2 = 1:10*10, col = 1:10)

   # example taken from post by Hans Borcher:
   # https://stat.ethz.ch/pipermail/r-help/2009-July/205728.html
   # Note setting of aspect ratio to 1 first.
   curve(sin(x), 0, pi, col="blue", asp=1)
   draw.arc(pi/2, 0, 1, deg1=45, deg2=135, col="red")

}
\keyword{misc}
