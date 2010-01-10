\name{twoord.plot}
\alias{twoord.plot}
\title{ Plot with two ordinates }
\description{
 Two sets of values are displayed on the same plot with different ordinate
 scales on the left and right.
}
\usage{
 twoord.plot(lx,ly,rx,ry,data=NULL,xlim=NULL,lylim=NULL,rylim=NULL,
 mar=c(5,4,4,4),lcol=1,rcol=2,xlab="",ylab="",rylab="",lpch=1,rpch=2,
 type="b",xtickpos=NULL,xticklab=NULL,halfwidth=0.4,axislab.cex=1,...)
}
\arguments{
 \item{lx,ly,rx,ry}{y and optional x values for the plot}
 \item{data}{an optional data frame from which to obtain the above values}
 \item{xlim}{optional x limits as in \samp{plot}}
 \item{lylim,rylim}{optional y limits for the left and right axes
  respectively}
 \item{mar}{optional margin adjustment, defaults to \samp{c(5,4,4,4)}}
 \item{lcol,rcol}{colors to distinguish the two sets of values}
 \item{xlab,ylab}{axis labels as in \samp{plot}}
 \item{rylab}{label for the right axis}
 \item{lpch,rpch}{plot symbols to distinguish the two sets of values}
 \item{type}{as in \samp{plot}}
 \item{xtickpos}{Optional positions for x-axis tick labels.}
 \item{xticklab}{Optional labels for x-axis. Useful for things like dates.}
 \item{halfwidth}{Half the width of the bars in user units. The bars are
  centered on successive integers if no \samp{x} values are supplied.}
 \item{axislab.cex}{Character expansion for the axis labels and tick labels.}
 \item{...}{additional arguments passed to \samp{plot}.}
}
\value{nil}
\details{
 \samp{twoord.plot} automates the process of displaying two sets of
 values that have different ranges on the same plot. It is principally
 useful in illustrating some relationship between the values across the
 observations. It is assumed that the \samp{lx} and \samp{rx} values
 are at least adjacent, and probably overlapping.
 
 It is best to pass all the arguments \samp{lx, ly, rx, ry}, but the
 function will attempt to substitute sensible x values if one or two
 are missing.

 If at least one of the \samp{type} arguments is "bar", bars will be plotted instead of
 points or lines. It is best to plot the bars first (i.e. relative to the left axis)
 if the other type is points or lines, as the bars will usually obscure at least some
 of the points or lines. Using NA for the color of the bars will partially correct
 this. If both types are to be bars, remember to pass somewhat different x values or
 the bars will be overlaid.

 Note that more values can be added to the plot using \samp{points} or \samp{lines},
 but remember that these will be plotted relative to the right ordinate, and the
 \samp{rylim} argument may need to be set so that more extreme points in subsequent
 series will be within the plot area.
}
\note{
 There are many objections to the use of plots with two different 
 ordinate scales, and some of them are even sensible and supported by 
 controlled observation. Many of the objections rest on assertions that the 
 spatial arrangement of the values plotted will override all other 
 evidence. Here are two:
 
 The viewer will assume that the vertical position of the data points 
 indicates a quantitative relationship.
 
 To some extent. It is probably not a good idea to have the spatial 
 relationship of the points opposed to their numerical relationship. That 
 is to say, if one set of values is in the range of 0-10 and the other 
 20-100, it is best to arrange the plot so that the latter values are 
 not plotted below the former.
 
 The viewer will assume that an intersection of lines indicates an 
 intersection of values.
 
 If the visual elements representing values can be arranged to avoid 
 intersections, so much the better. Many people have no trouble 
 distinguishing which visual elements are linked to which axis as long as 
 they are both coded similarly, usually with colors and/or symbols. In the 
 special case where there is an underlying relationship between the two 
 such as the probability of that value occurring under some conditions, it 
 may help to mark the point(s) where this occurs.
 
 It may be useful to consider \samp{gap.plot} as an alternative.
}
\author{Jim Lemon (thanks to Christophe Dutang for the idea of using bars
 and lines in the same plot, Clair Crossupton for pointing out that
 dates on the x-axis weren't very good and Jacob Kasper for the axis
 character expansion.)}
\seealso{\link{plot}}
\examples{
 twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
  1:15,rev(60:74)+rnorm(15),xlab="Sequence",
  ylab="Ascending values",rylab="Descending values",
  main="Plot with two ordinates - points and lines")
 twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
  1:15,rev(60:74)+rnorm(15),xlab="Sequence",
  ylab="Ascending values",rylab="Descending values",
  main="Plot with two ordinates - bars on the left",
  type=c("bar","l"),lcol=3,rcol=4)
 twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
  1:15,rev(60:74)+rnorm(15),xlab="Sequence",
  ylab="Ascending values",rylab="Descending values",
  main="Plot with two ordinates - bars on the right",
  type=c("b","bar"),lcol=2,rcol=NA,halfwidth=0.2)
 # histogram with density curve overlaid
 xhist<-hist(rnorm(100),plot=FALSE)
 xdens<-dnorm(seq(-2,2,by=0.05))
 twoord.plot(xhist$mids,xhist$counts,seq(-2,2,by=0.05),
 xdens,type=c("bar","l"),lcol=4,rcol=2,ylab="Counts",
 rylab="Density",main="Histogram and density curve",
 halfwidth=0.2)
}
\keyword{misc}
