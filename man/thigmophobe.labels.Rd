\name{thigmophobe.labels}
\title{thigmophobe.labels}
\usage{
 thigmophobe.labels(x,y,labels,xoffset=strwidth("F"),yoffset=strheight("I"),...)
}
\alias{thigmophobe.labels}
\arguments{
 \item{x,y}{Numeric data vectors. Typically the x/y coordinates of
  plotted points.}
 \item{labels}{A vector of strings that will be placed adjacent to
  each point. Defaults to the indices of the coordinates.}
 \item{xoffset}{The amount of space to offset the left- or right-aligned
  label from the point.}
 \item{yoffset}{The amount of space to offset the center-aligned
  label from the point.}
 \item{...}{additional arguments are passed to \code{text}}.
}
\description{
 \code{thigmophobe.labels} places labels adjacent to each point, 
  offsetting and aligning each in the direction returned by \code{thigmophobe}.}
}
\details{
 Typically used to automatically place labels on a scatterplot or 
 similar to avoid overlapping labels.
}
\value{
 A vector of directions away from the point nearest to each point.
}
\keyword{misc}
\authors{Jim Lemon}
\seealso{\code{thigmophobe}}
\examples{
 x<-rnorm(10)
 y<-rnorm(10)
 xlim<-range(x)
 xspace<-(xlim[2]-xlim[1])/20
 xlim<-c(xlim[1]-xspace,xlim[2]+xspace)
 ylim<-range(y)
 yspace<-(ylim[2]-ylim[1])/20
 ylim<-c(ylim[1]-yspace,ylim[2]+yspace)
 plotlabels<-
  c("one","two","three","four","five","six","seven","eight","nine","ten")
 plot(x=x,y=y,xlim=xlim,ylim=ylim)
 thigmophobe.labels(x,y,plotlabels,col=2:11)
}
