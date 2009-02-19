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
  Draws one or more arcs from \samp{angle1} to \samp{angle2}.
  If \samp{angle1} is numerically greater than \samp{angle2},
  then the angles are swapped.
}
\value{
  Returns a matrix of expanded arguments invisibly.
}
\author{Gabor Grothendieck}
\examples{
plot(1:10, asp = 1,main="Test draw.arc")
draw.arc(5, 5, 1:10/10, deg2 = 1:10*10, col = "blue")
draw.arc(8, 8, 1:10/10, deg2 = 1:10*10, col = 1:10)
}
\keyword{misc}
