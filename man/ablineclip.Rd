\name{ablineclip}
\alias{ablineclip}
\title{Add a straight line to a plot}
\description{
  As \code{abline}, but has arguments \code{x1,x2,y1,y2} as in \code{clip}.
}
\usage{
 ablineclip(a=NULL,b=NULL,h=NULL,v=NULL,reg=NULL,coef=NULL,untf=FALSE,
  x1=NULL,x2=NULL,y1=NULL,y2=NULL,...)
}
\arguments{
 \item{a}{Intercept.}
 \item{b}{Slope.}
 \item{h}{the x-value(s) for vertical line(s).}
 \item{v}{the y-value(s) for horizontal line(s).}
 \item{reg}{Fitted lm object. }
 \item{coef}{Coefficients, typically intercept and slope.}
 \item{untf}{How to plot on log coordinates, see \code{abline}.}
 \item{x1,x2,y1,y2}{Clipping limits, see \code{clip}.}
 \item{...}{Further arguments passed to \code{abline}.}
}
\details{
 \code{ablineclip} sets a new clipping region and then calls \code{abline}.
 If any of the four clipping limits is NULL, the values from \code{par("usr")}
 are substituted. After the call to \code{abline}, the old clipping region
 is restored. In order to make \code{clip} work, there is a call to \code{abline}
 that draws a line off the plot.
}
\value{
 None. Adds to the current plot.
}
\author{ Remko Duursma }
\seealso{ \code{\link{abline}} }
\examples{
 x <- rnorm(100)
 y <- x + rnorm(100)
 lmfit <- lm(y~x)
 plot(x,y,xlim=c(-3.5,3.5))
 ablineclip(lmfit,x1=-2,x2=2,lty=2)
 ablineclip(h=0,x1=-2,x2=2,lty=3,col="red")
 ablineclip(v=0,y1=-2.5,y2=1.5,lty=4,col="green")
}
\keyword{ aplot }