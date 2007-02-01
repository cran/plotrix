\name{thigmophobe.labels}
\title{Place labels away from the nearest point}
\usage{
 thigmophobe.labels(x,y,labels=NULL,text.pos=NULL,...)
}
\alias{thigmophobe.labels}
\arguments{
 \item{x,y}{Numeric data vectors or a list with two components. 
  Typically the x/y coordinates of plotted points.}
 \item{labels}{A vector of strings that will be placed adjacent to
  each point. Defaults to the indices of the coordinates.}
 \item{text.pos}{An optional vector of text positions (see 
 \code{\link{text}}).}
 \item{...}{additional arguments are passed to \code{text}}.
}
\description{
 \code{thigmophobe.labels} places labels adjacent to each point, 
  offsetting each label in the direction returned by 
  \code{thigmophobe}.}
}
\details{
 Typically used to automatically place labels on a scatterplot or 
 similar to avoid overlapping labels. \code{thigmophobe.labels}
 will sometimes place a label off the plot or fail to separate
 labels in clusters of points. The user can manually adjust the
 errant labels by running \code{thigmophobe} first and saving
 the returned vector. Then modify the position values to place
 the labels properly and pass the edited vector to
 \code{thigmophobe.labels} as the \code{text.pos} argument. This
 takes precedence over the positions calculated by \code{thigmophobe}.
}
\value{
 A vector of directions away from the point nearest to each point.
}
\keyword{misc}
\author{Jim Lemon}
\seealso{\code{\link{thigmophobe}}, \code{\link{text}}}
\examples{
 x<-rnorm(20)
 y<-rnorm(20)
 xlim<-range(x)
 xspace<-(xlim[2]-xlim[1])/20
 xlim<-c(xlim[1]-xspace,xlim[2]+xspace)
 ylim<-range(y)
 yspace<-(ylim[2]-ylim[1])/20
 ylim<-c(ylim[1]-yspace,ylim[2]+yspace)
 plotlabels<-
  c("one","two","three","four","five","six","seven","eight","nine","ten",
  "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen",
  "eighteen","nineteen","twenty")
 plot(x=x,y=y,xlim=xlim,ylim=ylim,main="Test thigmophobe.labels")
 # skip the almost invisible yellow label
 thigmophobe.labels(x,y,plotlabels,col=c(2:6,8:12))
}
