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
 radial.plot(lengths,radial.pos=NULL,labels,label.pos=NULL,
 start=0,clockwise=FALSE,rp.type="r",label.prop=1.1,main="",xlab="",ylab="",
 line.col=par("fg"),mar=c(2,2,3,2),show.grid=TRUE,show.radial.grid=TRUE,
 grid.col="gray",grid.bg="transparent",grid.left=FALSE,grid.unit=NULL,
 point.symbols=NULL,point.col=NULL,show.centroid=FALSE,radial.lim=NULL,
 poly.col=NA,...)
}
\arguments{
 \item{lengths}{A numeric data vector or matrix. If \code{lengths}
  is a matrix, the rows will be considered separate data vectors.}
 \item{radial.pos}{A numeric vector or matrix of positions in radians. 
  These are interpreted as beginning at the right (0 radians) and moving
  counterclockwise. If \code{radial.pos} is a matrix, the rows must 
  correspond to rows of \code{lengths}.}
 \item{labels}{Character strings to be placed at the outer ends of
  the lines. If set to NA, will suppress printing of labels,
  but if missing, the radial positions will be used.}
 \item{label.pos}{The positions of the labels around the plot in radians.}
 \item{start}{Where to place the starting (zero) point. Defaults to the
  3 o'clock position.}
 \item{clockwise}{Whether to interpret positive positions as clockwise from
  the starting point. The default is counterclockwise.}
 \item{rp.type}{Whether to draw (r)adial lines, a (p)olygon, (s)ymbols
  or some combination of these.}
 \item{label.prop}{The label position radius as a proportion of the 
  maximum line length.}
 \item{main}{The title for the plot.}
 \item{xlab,ylab}{Normally x and y axis labels are suppressed.}
 \item{line.col}{The color of the radial lines or polygons drawn.}
 \item{mar}{Margins for the plot. Allows the user to leave space for
  legends, long labels, etc.}
 \item{show.grid}{Logical - whether to draw a circular grid.}
 \item{show.radial.grid}{Whether to draw radial lines to the plot labels.}
 \item{grid.col}{Color of the circular grid.}
 \item{grid.bg}{Fill color of above.}
 \item{grid.left}{Whether to place the radial grid labels on the left side.}
 \item{grid.unit}{Optional unit description for the grid.}
 \item{point.symbols}{The symbols for plotting (as in pch).}
 \item{point.col}{Colors for the symbols.}
 \item{show.centroid}{Whether to display a centroid.}
 \item{radial.lim}{The range of the grid circle. Defaults
  to \code{range(pretty(lengths))}.}
 \item{poly.col}{Fill color if polygons are drawn.}
 \item{...}{Additional arguments are passed to \code{plot}.}
}
\value{
 nil
}
\details{
 \code{radial.plot} displays a plot of radial lines, polygon(s),
 symbols or a combination of these centered at the midpoint of
 the plot frame, the lengths, vertices or positions corresponding
 to the numeric magnitudes of the data values. If \code{show.centroid}
 is TRUE, an enlarged point at the centroid of values is displayed. The
 centroid is calculated as the average of x and y values unless 
 \code{rp.type="p"}. In this case, the barycenter of the polygon is
 calculated. Make sure that these suit your purpose, otherwise calculate
 the centroid that you really want and add it with the \code{points} function.
  
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
 oldpar<-radial.plot(testlen,testpos,main="Test Radial Lines",line.col="red",lwd=3)
 testlen<-c(sin(seq(0,1.98*pi,length=100))+2+rnorm(100)/10)
 testpos<-seq(0,1.98*pi,length=100)
 radial.plot(testlen,testpos,rp.type="p",main="Test Polygon",line.col="blue")
 # now do a 12 o'clock start with clockwise positive
 radial.plot(testlen,testpos,start=pi/2,clockwise=TRUE,
  rp.type="s",main="Test Symbols (clockwise)",
  point.symbols=16,point.col="green",show.centroid=TRUE)
 # one without the circular grid and multiple polygons
 posmat<-matrix(sample(2:9,30,TRUE),nrow=3)
 radial.plot(posmat,labels=paste("X",1:10,sep=""),rp.type="p",
  main="Spiderweb plot",line.col=2:4,show.grid=FALSE,
  radial.lim=c(0,10))
 # dissolved ions in water
 ions<-c(3.2,5,1,3.1,2.1,4.5)
 ion.names<-c("Na","Ca","Mg","Cl","HCO3","SO4")
 radial.plot(ions,labels=ion.names,rp.type="p",main="Dissolved ions in water",
  grid.unit="meq/l",radial.lim=c(0,5),poly.col="yellow")
 par(xpd=oldpar$xpd,mar=oldpar$mar,pty=oldpar$pty)
 # reset the margins
 par(mar=c(5,4,4,2))
}
\keyword{misc}
