\name{feather.plot}
\alias{feather.plot}
\title{ Display vectors along a horizontal reference line. }
\description{
 Displays vectors along a line usually representing time or position.
}
\usage{
 feather.plot(r,theta,xpos,yref=0,use.arrows=TRUE,
 col.refline="lightgray",fp.type="s",main="",xlab="",ylab="",
 xlabels=NULL,...)
}
\arguments{
 \item{r}{radii of vectors}
 \item{theta}{direction of vectors in radians}
 \item{xpos}{where to start each vector along the reference line}
 \item{yref}{vertical position to place the reference line}
 \item{use.arrows}{whether to put arrow heads on the ends of the vectors}
 \item{col.refline}{the color of the reference line}
 \item{fp.type}{whether to use "standard" coordinates (begin at the
  right and move counterclockwise) or "meteorological" coordinates
  (begin at the top and move clockwise) when interpreting the values
  of \samp{theta}}
 \item{main}{the title of the plot}
 \item{xlab}{the label for the reference line}
 \item{ylab}{the label for the vertical axis}
 \item{xlabels}{optional labels for the reference line}
 \item{...}{additional arguments passed to \samp{arrows} or \samp{segments}}
}
\details{
 This function places vectors of length \samp{r} and angle \samp{theta} 
 along a reference line that may represent time or position or some other
 value. The user is responsible for spacing the vectors so that they do
 not overlap if this is desired.
}
\value{nil}
\author{Jim Lemon, Eduardo Klein}
\seealso{\link{spread.labels}}
\examples{
 feather.plot(0.6+rnorm(8)/5,seq(0,7*pi/4,by=pi/4),1:8,
  main="Standard Coordinates",xlab="Time",ylab="Value")
 if(dev.interactive()) par(ask=TRUE)
 feather.plot(0.6+rnorm(8)/5,seq(0,7*pi/4,by=pi/4),1:8,
  main="Meteorological Coordinates",xlab="Time",ylab="Value",
  fp.type="m",xlabels=TRUE)
 par(ask=FALSE)
}
\keyword{misc}
