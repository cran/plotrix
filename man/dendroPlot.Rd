\name{dendroPlot}
\alias{dendroPlot}
\title{ Display categorical distributions as dendrites }
\description{
 Display the distributions of one or more sets of points as branching
  (dendritic) clusters.
}
\usage{
 dendroPlot(x,breaks=NULL,pch=1,cex=1,nudge=NA,setlabels=NA,...)
}
\arguments{
 \item{x}{A list, matrix or data frame of numeric or factor or character
  columns.}
 \item{breaks}{A list of cutpoints to transform numeric values into factors.}
 \item{pch}{Symbol(s) to use in plotting the values.}
 \item{cex}{Size of the symbol(s) to use in plotting.}
 \item{nudge}{The amount to set each consecutive value in a category away from
  the center of the dendrite.}
 \item{setlabels}{Labels to place along the abcissa to identify the sets.}
 \item{...}{Other arguments passed to plot.}
}
\details{
 \samp{dendroPlot} displays the distributions of categorical values as stacks
 of "branches". The lengths of the branches show the number of values in each
 category, rather like the opposed bars in a pyramid plot, except that there
 is no separation of groups. The distribution of numeric values can also be
 displayed by passing a set of breakpoints to categorize the values. The
 breakpoints will usually be equidistant, but unevenly spaced breakpoints can
 be passed.

 Note that in the example, the breakpoints for the first element are closer
 together than for the last two elements. This causes the uniform sample with
 its range of 1 to 2 to be spread out into the range 1 to 10. If the vertical
 spread of the distributions had been made equal to the numeric values, the
 branches for the two last elements would have been spread out to ten times
 the spacing of those of the first element. When comparing distributions with
 very different ranges it is difficult to display both category counts and
 ranges of values.

 Each successive point in a category is \samp{nudge}d away from the center
 of the dendrite. If \samp{nudge} has more than one value, the points will be
 nudged up and down to enable closer packing. The aspect ratio of the plot,
 the character expansion and the nudging will have to be adjusted to give the
 best point spacing for most dendroPlots.
}
\value{nil}
\author{Jim Lemon}
\examples{
 x11(height=4)
 x<-list(runif(90,1,2),factor(sample(LETTERS,100,TRUE)),rnorm(80,mean=5))
 dendroPlot(x,breaks=list(seq(1,2,by=0.1),0,0:10),nudge=c(0.03,0.3),
 xlab="Groups",ylab="Counts",main="Test dendroPlot")
}
\keyword{misc}
