\name{twoord.plot}
\alias{twoord.plot}
\title{ Plot with two ordinates }
\description{
 Two sets of values are displayed on the same plot with different ordinate
 scales on the left and right.
}
\usage{
 twoord.plot(lx,ly,rx,ry,data=NULL,xlim=NULL,lylim=NULL,rylim=NULL,
 mar=NULL,lcol=1,rcol=2,xlab="",ylab="",rylab="",lpch=1,rpch=2,
 type="b",...)
}
\arguments{
 \item{lx,ly,rx,ry}{y and optional x values for the plot}
 \item{data}{an optional data frame from which to obtain the above values}
 \item{xlim}{optional x limits as in \code{plot}}
 \item{lylim,rylim}{optional y limits for the left and right axes
  respectively}
 \item{mar}{optional margin adjustment, defaults to \code{c(5,4,4,4)}}
 \item{lcol,rcol}{colors to distinguish the two sets of values}
 \item{xlab,ylab}{axis labels as in \code{plot}}
 \item{rylab}{label for the right axis}
 \item{lpch,rpch}{plot symbols to distinguish the two sets of values}
 \item{type}{as in \code{plot}}
 \item{...}{additional arguments passed to \code{axis}.}
}
\value{nil}
\details{
 \code{twoord.plot} automates the process of displaying two sets of
 values that have different ranges on the same plot. It is principally
 useful in illustrating some relationship between the values across the
 observations. It is assumed that the \code{lx} and \code{rx} values
 are at least adjacent, and probably overlapping.
 
 It is best to pass all the arguments \code{lx, ly, rx, ry}, but the
 function will attempt to substitute sensible x values if one or two
 are missing.
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
 
 It may be useful to consider \code{gap.plot} as an alternative.
}
\author{Jim Lemon}
\seealso{\code{\link{plot}}}
\examples{
 twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
  1:15,rev(60:74)+rnorm(15),xlab="Sequence",
  ylab="Ascending values",rylab="Descending values",
  main="Test of twoord.plot")
}
\keyword{misc}
