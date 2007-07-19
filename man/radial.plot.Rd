\name{radial.plot}
\alias{radial.plot}
\title{
 Plot values on a circular grid of 0 to 2*pi radians.
}
\description{
 Plot numeric values as distances from the center of a circular field in the
 directions defined by angles in radians.
}
\usage{
 radial.plot(lengths,radial.pos,labels,label.pos,rp.type="r",
 label.prop=1.1,main="",xlab="",ylab="",line.col=par("fg"),mar=c(2,2,3,2),
 show.grid=TRUE,show.radial.grid=TRUE,grid.col="gray",grid.bg="transparent",
 point.symbols=NULL,point.col=NULL,show.centroid=FALSE,radial.lim=NULL,...)
}
\arguments{
 \item{lengths}{A numeric data vector or matrix. If \code{lengths}
  is a matrix, the rows will be considered separate data vectors.}
 \item{radial.pos}{A numeric vector or matrix of positions in radians. 
  These are interpreted as beginning at the right (0 radians) and moving
  counterclockwise. If \code{radial.pos} is a matrix, the rows must 
  correspond to rows of \code{lengths}.}
 \item{labels}{character strings to be placed at the outer ends of
  the lines. If set to NULL, will suppress printing of labels,
  but if missing, the radial positions will be used.}
 \item{label.pos}{the positions of the labels as distance from the
  'center' of the plot. If missing, they will be placed at 1.1
  times the maximum of \code{lengths}.}
 \item{rp.type}{whether to draw (r)adial lines, a (p)olygon, (s)ymbols
  or some combination of these.}
 \item{label.prop}{the label position radius as a proportion of the 
  maximum line length.}
 \item{main}{The title for the plot.}
 \item{xlab,ylab}{Normally x and y axis labels are suppressed.}
 \item{line.col}{The color of the radial lines or polygons drawn.}
 \item{mar}{Margins for the plot. Allows the user to leave space for
  legends, etc.}
 \item{show.grid}{Logical - whether to draw a circular grid.}
 \item{show.radial.grid}{Whether to draw radial lines to the plot labels.}
 \item{grid.col}{Color of the circular grid.}
 \item{grid.bg}{Fill color of above.}
 \item{point.symbols}{the symbols for plotting (as in pch).}
 \item{point.col}{colors for the symbols.}
 \item{show.centroid}{whether to display a centroid.}
 \item{radial.lim}{the range of the grid circle. Defaults
  to \code{range(pretty(lengths))}.}
 \item{...}{additional arguments are passed to \code{plot}.}
}
\value{
 nil
}
\details{
 \code{radial.plot} displays a plot of radial lines, polygon(s),
 symbols or a combination of these centered at the midpoint of
 the plot frame, the lengths, vertices or positions corresponding
 to the numeric magnitudes of the data values. If \code{show.centroid}
 is TRUE, an enlarged point at the centroid of values is displayed.
  
 If the user wants to plot several sets of lines, points or symbols by
 passing matrices or data frames of \code{lengths} and \code{radial.pos},
 remember that these will be grouped by row, so transpose if the data are
 grouped by columns.

 The radial.plot family of plots is useful for illustrating
 cyclic data such as wind direction or speed (but see \code{oz.windrose}
 for both), activity at different times of the day, and so on. While 
 \code{radial.plot} actually does the plotting, another function is usually 
 called for specific types of cyclic data. Note that if the observations
 are not taken at equal intervals around the circle, the centroid may not
 mean much.
}
\author{Jim Lemon}
\seealso{\code{\link{polar.plot}},\code{\link{clock24.plot}}}
\examples{
 testlen<-rnorm(10)*2+5
 testpos<-seq(0,18*pi/10,length=10)
 testlab<-letters[1:10]
 radial.plot(testlen,testpos,main="Test Radial Lines",line.col="red",lwd=3)
 if(dev.interactive()) par(ask=TRUE)
 testlen<-c(sin(seq(0,1.98*pi,length=100))+2+rnorm(100)/10)
 testpos<-seq(0,1.98*pi,length=100)
 radial.plot(testlen,testpos,rp.type="p",main="Test Polygon",line.col="blue")
 radial.plot(testlen,testpos,rp.type="s",main="Test Symbols",point.symbols=16,
  point.col="green",show.centroid=TRUE)
 par(ask=FALSE)
}
\keyword{misc}
