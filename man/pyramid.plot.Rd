\name{pyramid.plot}
\alias{pyramid.plot}
\title{Pyramid plot}
\description{
 Displays a pyramid (opposing horizontal bar) plot on the current
  graphics device.
}
\usage{
 pyramid.plot(xy,xx,labels, top.labels=c("Male","Age","Female"),
  main="",xycol,xxcol,gap=1)
}
\arguments{
  \item{xy,xx}{Vectors of percentages both of which should total 100.}
  \item{labels}{Labels for the categories represented by each pair of bars.} 
  \item{top.labels}{The two categories represented on the left and right
   sides of the plot.}
  \item{main}{Optional title for the plot.}
  \item{xycol,xxcol}{Color(s) for the left and right sets of bars.}
  \item{gap}{One half of the space between the two sets of bars for the
   \code{labels} in user units.}
}
\details{
 \code{pyramid.plot} is principally intended for population pyramids,
  although it can display other types of opposed bar charts with suitable
  modification of the arguments. The percentage display is hardwired into
  the function, but minor programming would allow one to use proportions
  or other metrics. The default gap of two units is usually satisfactory
  for the four to six percent range of most bars on population pyramids.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{rect}}}
\examples{
 xy.pop<-c(3.2,3.5,3.6,3.6,3.5,3.5,3.9,3.7,3.9,3.5,3.2,2.8,2.2,1.8,
  1.5,1.3,0.7,0.4)
 xx.pop<-c(3.2,3.4,3.5,3.5,3.5,3.7,4,3.8,3.9,3.6,3.2,2.5,2,1.7,1.5,
  1.3,1,0.8)
 agelabels<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
  "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
  "75-79","80-44","85+")
 xycol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
 xxcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)
 pyramid.plot(xy.pop,xx.pop,labels=agelabels,
  main="Australian population pyramid 2002",xycol=xycol,xxcol=xxcol)
}
\keyword{misc}
