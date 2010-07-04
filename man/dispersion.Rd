\name{dispersion}
\alias{dispersion}
\alias{dispbars}
\title{ Display a measure of dispersion }
\description{
 Display line/cap bars at specified points on a plot representing
  measures of dispersion.
}
\usage{
 dispersion(x,y,ulim,llim=ulim,intervals=TRUE,arrow.cap=0.01,arrow.gap=NA,
  type="a",fill=NA,lty=NA,pch=NA,border=NA,...)
 dispbars(x,y,ulim,llim=ulim,intervals=TRUE,arrow.cap=0.01,arrow.gap=NA,...)
}
\arguments{
 \item{x,y}{x and y position of the centers of the bars}
 \item{ulim,llim}{The extent of the dispersion measures.}
 \item{arrow.cap}{The width of the cap at the outer end of each bar
  as a proportion of the width of the plot.}
 \item{arrow.gap}{The gap to leave at the inner end of each bar.
  Defaults to two thirds of the height of a capital "O".}
 \item{intervals}{Whether the limits are intervals (TRUE) or absolute values
  (FALSE).}
 \item{type}{What type of display to use.}
 \item{fill}{Color to fill between the lines if \samp{type} is not NULL and
  \samp{fill} is not NA.}
 \item{lty}{Line type for redrawing the lines if necessary.}
 \item{pch}{Symbol for redrawing the points if necessary.}
 \item{border}{Line type for drawing a border on the confidence band.}
 \item{...}{additional arguments passed to \samp{arrows} or \samp{lines}
  depending upon \samp{type}.}
}
\details{
 \samp{dispersion} displays a measure of dispersion on an existing plot.
 Currently it will display either vertical lines with caps (error bars) or lines
 that form a "confidence band" around a line of central tendency. If \samp{fill}
 is not NA and \samp{type} is \samp{"l"}, a polygon will be drawn between the
 confidence lines. Remember that any points or lines within the confidence band
 will be obscured, so these will have to be redrawn.

 The \samp{intervals} argument allows the user to pass the limits as either intervals (the default) or absolute values.

 \samp{dispbars} is now a call to \samp{dispersion} with \samp{type} set to
 \samp{a}.
 
 If \samp{arrow.gap} is greater than or equal to the upper or lower
 limit for a bar, \samp{segments} is used to draw the upper and
 lower caps with no bars to avoid zero length arrows.
}
\value{nil}
\author{Jim Lemon}
\seealso{\link{arrows}, \link{segments},\link{lines}}
\examples{
 disptest<-matrix(rnorm(200),nrow=20)
 disptest.means<-rowMeans(disptest)
 row.order<-order(disptest.means)
 se.disptest<-unlist(apply(disptest,1,std.error))
 plot(disptest.means[row.order],main="Dispersion as error bars",
  ylim=c(min(disptest.means-se.disptest),max(disptest.means+se.disptest)),
  xlab="Occasion",ylab="Value")
 dispersion(1:20,disptest.means[row.order],se.disptest[row.order])
 plot(disptest.means[row.order],main="Dispersion as confidence band",
  ylim=c(min(disptest.means-se.disptest),max(disptest.means+se.disptest)),
  xlab="Occasion",ylab="Value")
 dispersion(1:20,disptest.means[row.order],se.disptest[row.order],type="l",
  fill="#eeccee",lty=2)
 # remember to redraw the points
 points(disptest.means[row.order])
}
\keyword{misc}
