\name{pie3D}
\alias{pie3D}
\title{Display a 3D pie chart}
\description{
 Displays a 3D pie chart with optional labels.
}
\usage{
 pie3D(x,edges=100,radius=1,height=0.3,theta=pi/6,border=par("fg"),
  col=NULL,labels=NULL,labelpos=NULL,labelcol=par("fg"),labelcex=1.5,
  explode=0,...)
}
\arguments{
 \item{x}{a numeric vector for which each value will be a sector}
 \item{edges}{the number of lines forming an ellipse}
 \item{radius}{the radius of the pie in user units}
 \item{height}{the height of the pie in user units}
 \item{theta}{The angle of viewing in radians}
 \item{border}{The color of the sector border lines}
 \item{col}{The colors of the sectors}
 \item{labels}{Optional labels for each sector}
 \item{labelpos}{Optional positions for the labels}
 \item{labelcol}{The color of the labels}
 \item{labelcex}{The character expansion factor for the labels}
 \item{explode}{The amount to "explode" the pie in user units}
 \item{...}{graphical parameters passed to \code{plot}}
}
\value{
 The bisecting angle of the sectors in radians.
}
\details{
 \code{pie3D} scales the values in \code{x} so that they total 2*pi,
 dropping zeros and NAs. It then displays an empty plot, calculates
 the sequence for drawing the sectors and calls \code{draw.tilted.sector}
 to draw each sector. If labels are supplied, it will call \code{pie3D.label}
 to place a label for each sector. If supplied, the number of labels, label 
 positions and sector colors must be at least equal to the number of values 
 in \code{x}. If the labels are long, it may help to reduce the radius of
 the pie.
}
\author{Jim Lemon}
\seealso{\code{\link{pie3D.labels}}, \code{\link{draw.tilted.sector}}}
\examples{
 pieval<-c(2,4,6,8)
 pie3D(pieval,labels=c("TWO","FOUR","SIX","EIGHT"),explode=0.1,main="3D PIE")
}
\keyword{misc}
