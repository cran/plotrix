\name{drawNestedBars}
\alias{drawNestedBars}
\title{Display nested bars}
\description{Displays the nested bars for barNest.}
\usage{
 drawNestedBars(x,start,end,shrink=0.1,errbars=FALSE,col=NA,
 labelcex=1,labely=NA,showbars=TRUE,showlabels=TRUE,arrow.cap=0.01)
}
\arguments{
 \item{x}{One level of the breakdown produced by \samp{brkdnNest}.}
 \item{start,end}{The left and right x coordinates for the bar or group of bars
  to be displayed.}
 \item{shrink}{The proportion to shrink the width of the bars at each level.}
 \item{errbars}{Whether to display error bars on the bars.}
 \item{col}{The colors to use to fill the bars. See Details.}
 \item{labelcex}{Character size for the group labels.}
 \item{labely}{The position of the group labels in user units. This is calculated
  by \samp{barNest}.}
 \item{showbars}{Whether to display the bars at any levels above the last.}
 \item{showlabels}{Whether to display the labels below the bars.}
 \item{arrow.cap}{The width of the "cap" on error bars in user units,
  defaulting to 0.01 of the width of the plot.}
}
\value{nil}
\details{
 \samp{drawNestedBars} displays the bars for one level of the nested
 breakdown performed by \samp{brkdnNest}. It is unlikely to be useful for
 anything else.

 The combination of \samp{showlabels=TRUE} and \samp{showbars=FALSE} allows
 the display of all of the labels below the plot with only the last set of
 bars being displayed.
}
\author{Jim Lemon and Ofir Levy}
\seealso{\link{brkdnNest}, \link{drawNestedBars}}
\keyword{misc}
