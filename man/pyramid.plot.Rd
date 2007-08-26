\name{pyramid.plot}
\alias{pyramid.plot}
\title{Pyramid plot}
\description{
 Displays a pyramid (opposed horizontal bar) plot on the current
  graphics device.
}
\usage{
 pyramid.plot(xy,xx,labels,top.labels=c("Male","Age","Female"),
  main="",unit="\%",xycol,xxcol,gap=1,labelcex=1,mark.cat=NA,add=FALSE)
}
\arguments{
  \item{xy,xx}{Vectors of percentages both of which should total 100.}
  \item{labels}{Labels for the categories represented by each pair of bars.} 
  \item{top.labels}{The two categories represented on the left and right
   sides of the plot and a heading for the labels up the center.}
  \item{main}{Optional title for the plot.}
  \item{unit}{The label for the units of the plot.}
  \item{xycol,xxcol}{Color(s) for the left and right sets of bars.}
  \item{gap}{One half of the space between the two sets of bars for the
   \code{labels} in user units.}
  \item{labelcex}{Expansion for the category labels.}
  \item{mark.cat}{If an integer equal to the index of one of the labels
   is passed, that label will have a rectangle drawn around it.}
  \item{add}{Whether to add bars to an existing plot. Usually this
   involves overplotting a second set of bars, perhaps transparent.}
}
\details{
 \code{pyramid.plot} is principally intended for population pyramids,
  although it can display other types of opposed bar charts with suitable
  modification of the arguments. If the user wants a different unit for
  the display, just change \code{unit} accordingly. The default gap of 
  two units is usually satisfactory for the four to six percent range 
  of most bars on population pyramids.
  
 The \code{add} argument allows one or more sets of bars to be plotted
  on an existing plot. If these are not transparent, any bar that is
  shorter than the bar that overplots it will dissapear.
  
 In order to add bars, the function cannot reset the initial margin values
 or the new bars will not plot properly. To automatically reset the plot
 margins, call the function as in the example.
}
\value{
 The return value of \code{par("mar")} when the function was called.
}
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
 par(mar=pyramid.plot(xy.pop,xx.pop,labels=agelabels,
  main="Australian population pyramid 2002",xycol=xycol,xxcol=xxcol))
}
\keyword{misc}
