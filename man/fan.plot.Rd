\name{fan.plot}
\alias{fan.plot}
\title{ Display a fan plot }
\description{
 Displays numerical values as the arcs of overlapping sectors.
}
\usage{
 fan.plot(x,edges=200,radius=1,col=NULL,align.at=NULL,max.span=NULL,
  labels=NULL,labelpos=NULL,label.radius=1.2,align="left",shrink=0.02,
  main="",ticks=NULL,include.sumx=FALSE,...)
}
\arguments{
 \item{x}{Vector of numbers.}
 \item{edges}{The number of edges with which to draw a circle.}
 \item{radius}{The radius of the sectors.}
 \item{col}{The colors with which to fill the sectors.}
 \item{align.at}{Where to align the sectors (see Details).}
 \item{max.span}{The angle of the maximal sector in radians. The default
  is to scale \samp{x} so that it sums to 2*pi.}
 \item{labels}{Labels placed around the sector arcs.}
 \item{labelpos}{Optional circumferential positions for the labels.}
 \item{label.radius}{How far away from the sectors the labels will
  be placed. May be a vector with a radius for each label.}
 \item{align}{Position of the alignment of sectors (see Details).}
 \item{shrink}{How much to shrink each successive sector in user units.}
 \item{main}{Optional title for the plot.}
 \item{ticks}{The number of ticks that would appear if the sectors were on
  a pie chart. Default is no ticks, TRUE gives the number of ticks equal to
  the integer sum of \samp{x}, which is fairly sensible if 
  \samp{x} is a vector of integers.}
 \item{include.sumx}{Whether to include the sum of all \samp{x} values
  as the largest sector.}
 \item{...}{Additional arguments passed to \samp{polygon}.}
}
\details{
 \samp{fan.plot} is now a call to fanPlot.
}
\value{
 The circumferential positions of the labels in radians. These are
 returned in order of decreasing size of the values plotted.
}
\author{Jim Lemon, Anupam Tyagi}
\keyword{misc}
