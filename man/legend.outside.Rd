\name{legend.outside}
\title{Display a legend outside the plot.}
\usage{
 legend.outside(side=1,x=NULL,y=NULL,look.for="plot",...)
}
\alias{legend.outside}
\arguments{
 \item{side}{The side of the plot on which the legend will be displayed.}
 \item{x,y}{Top left corner of the legend in user units. The legend will be
  automatically placed if they are not supplied. If these are incompatible 
  with \code{side} there will be trouble.}
 \item{look.for}{String to find last relevant plotting command.}
 \item{...}{Additional arguments passed to \code{legend}.}
}
\description{
 \code{legend.outside} displays a legend outside the plot, shrinking the plot
  to leave space.}
}
\value{The information returned from \code{legend}.}
\author{Jim Lemon}
\keyword{misc}
\seealso{\code{\link{last.call}}}
\examples{
 test1<-rnorm(10)
 test2<-rnorm(10)
 testylim<-range(c(test1,test2))
 plot(test1,type="p",ylim=testylim,pch=2,col=2,main="Test legend on bottom")
 points(test2,pch=3,col=3)
 if(dev.interactive()) {
  par(ask=TRUE)
  # run the legend round the plot
  legend.outside(legend=c("Red","Green"),pch=c(2,3),col=c(2,3))
  plot(test1,type="p",ylim=testylim,pch=2,col=2,main="Test legend on left")
  points(test2,pch=3,col=3)
  legend.outside(side=2,legend=c("Red","Green"),pch=c(2,3),col=c(2,3))
  plot(test1,type="p",ylim=testylim,pch=2,col=2,main="Test legend on top")
  points(test2,pch=3,col=3)
  legend.outside(side=3,legend=c("Red","Green"),pch=c(2,3),col=c(2,3))
  plot(test1,type="p",ylim=testylim,pch=2,col=2,main="Test legend on right")
  points(test2,pch=3,col=3)
  legend.outside(side=4,legend=c("Red","Green"),pch=c(2,3),col=c(2,3))
 }
 par(ask=FALSE)
}
