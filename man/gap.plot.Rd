\name{gap.plot}
\alias{gap.plot}
\title{Display a plot with a gap (missing range) on one axis}
\description{
 Displays a plot with a missing range.
}
\usage{
 gap.plot(x,y,gap,gap.side="y",xaxlab,xtics,yaxlab,ytics,
  col=par("col"),...)
}
\arguments{
 \item{x,y}{data values}
 \item{gap}{the range of values to be left out}
 \item{gap.side}{whether the gap is to be on the x or y axis}
 \item{xaxlab}{labels for the x axis ticks}
 \item{xtics}{position of the x axis ticks}
 \item{yaxlab}{labels for the y axis ticks}
 \item{ytics}{position of the y axis ticks}
 \item{col}{color(s) in which to plot the values}
 \item{...}{arguments passed to \code{plot}.}
}
\value{nil}
\details{
 Displays a plot omitting a range of values on one axis. Typically used 
 when there is a relatively large gap in the overall range of one set of
 values. The function warns the user if any values have been omitted by 
 being in the"gap". See \code{\link{axis.break}} for a brief discussion 
 of plotting on discontinuous coordinates.
}
\author{Jim Lemon and Ben Bolker}
\seealso{\code{\link{gap.barplot}}}
\examples{
 twogrp<-c(rnorm(10)+4,rnorm(10)+20)
 gap.plot(twogrp,gap=c(8,16),xlab="Index",ylab="Group values",
  main="Plot gap on Y axis")
 if(dev.interactive()) par(ask=TRUE)
 gap.plot(twogrp,rnorm(20),gap=c(8,16),gap.side="x",xlab="X values",
  xtics=c(4,7,17,20),ylab="Y values",main="Plot gap on X axis") 
 par(ask=FALSE)
}
\keyword{misc}
