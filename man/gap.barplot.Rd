\name{gap.barplot}
\alias{gap.barplot}
\title{Display a barplot with a gap (missing range) on one axis}
\description{
 Displays a barplot with a missing range.
}
\usage{
 gap.barplot(y,gap,xaxlab,xtics,yaxlab,ytics,ylab,col,...)
}
\arguments{
 \item{y}{data values}
 \item{gap}{the range of values to be left out}
 \item{xaxlab}{labels for the x axis ticks}
 \item{xtics}{position of the x axis ticks}
 \item{yaxlab}{labels for the y axis ticks}
 \item{ytics}{position of the y axis ticks}
 \item{ylab}{label for the y axis}
 \item{col}{color(s) in which to plot the values}
 \item{...}{arguments passed to \code{barplot}.}
}
\value{The center positions of the bars.}
\details{
 Displays a barplot omitting a range of values on the Y axis. Typically 
 used when there is a relatively large gap in the range of values 
 represented as bar heights. See \code{\link{axis.break}} for a brief 
 discussion of plotting on discontinuous coordinates.
}
\author{Jim Lemon}
\seealso{\code{\link{gap.barplot}}}
\examples{
 twogrp<-c(rnorm(10)+4,rnorm(10)+20)
 gap.barplot(twogrp,gap=c(8,16),xlab="Index",ytics=c(3,6,17,20),
  ylab="Group values",main="Barplot with gap")
}
\keyword{misc}
