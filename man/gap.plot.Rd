\name{gap.plot}
\alias{gap.plot}
\title{Display a plot with one or two gaps (missing ranges) on one axis}
\description{
 Displays a plot with one or two missing ranges on one of the axes.
}
\usage{
 gap.plot(x,y,gap,gap.axis="y",xticlab,xtics=NA,yticlab,ytics=NA,
  col=rep(par("col"),length(x)),xlim,ylim,pch=rep(1,length(x)),...)
}
\arguments{
 \item{x,y}{data values}
 \item{gap}{the range(s) of values to be left out}
 \item{gap.axis}{whether the gaps are to be on the x or y axis}
 \item{xticlab}{labels for the x axis ticks}
 \item{xtics}{position of the x axis ticks}
 \item{yticlab}{labels for the y axis ticks}
 \item{ytics}{position of the y axis ticks}
 \item{col}{color(s) in which to plot the values}
 \item{xlim,ylim}{the plot limits.}
 \item{pch}{symbols to use in plotting.}
 \item{...}{arguments passed to \code{plot}.}
}
\value{nil}
\details{
 Displays a plot omitting one or two ranges of values on one axis. 
 Typically used when there is a relatively large gap or two in the overall 
 range of one set of values, often because of outliers. The function warns
 the user if any values have been omitted by being in the "gap". See 
 \code{\link{axis.break}} for a brief discussion of plotting on 
 discontinuous coordinates.
 
 If at least four values are passed in \code{gap}, the first four will be
 used to calculate two "gaps" in the plot instead of one. The function does
 not check whether these values are sensible, so it is quite easy to
 ask for a very silly plot.
 
 The default ticks are usually not ideal, and most users will want to pass
 their own tick positions and perhaps labels.
}
\author{Jim Lemon and Ben Bolker}
\seealso{\code{\link{gap.barplot}}}
\examples{
 twogrp<-c(rnorm(5)+4,rnorm(5)+20,rnorm(5)+5,rnorm(5)+22)
 gpcol<-c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)
 gap.plot(twogrp,gap=c(8,16),xlab="Index",ylab="Group values",
  main="Plot gap on Y axis",col=gpcol)
 gap.plot(twogrp,rnorm(20),gap=c(8,16),gap.axis="x",xlab="X values",
  xtics=c(4,7,17,20),ylab="Y values",main="Plot gap on X axis") 
threegrp <- c(twogrp,rnorm(10)+40)
gap.plot(threegrp,gap=c(8,16,25,35),
 xlab="X values",ylab="Y values",
 main="Test two gap plot",
 ytics=c(4,6,18,20,22,38,40),
 pch=c(rep(2,10),rep(3,10),rep(4,10)),
 col=c(rep(2,10),rep(3,10),rep(4,10)))}
\keyword{misc}
