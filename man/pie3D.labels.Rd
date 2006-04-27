\name{pie3D.labels}
\alias{pie3D.labels}
\title{Display labels on a 3D pie chart}
\description{
 Displays labels on a 3D pie chart.
}
\usage{
 pie3D.labels(radialpos,radius=1,height=0.3,theta=pi/6,
  labels,labelcol=par("fg"),labelcex=1.5)
}
\arguments{
 \item{radialpos}{Position of the label in radians}
 \item{radius}{the radius of the pie in user units}
 \item{height}{the height of the pie in user units}
 \item{theta}{The angle of viewing in radians}
 \item{labels}{The label to display}
 \item{labelcol}{The color of the labels}
 \item{labelcex}{The character expansion factor for the labels}
}
\value{
 nil
}
\details{
 \code{pie3D.label} displays labels on a 3D pie chart. The positions
 of the labels may be given as angles in radians (usually the bisector of a 
 pie sector) or explicitly in user units. The latter overrides the calculation
 of the former.
}
\author{Jim Lemon}
\seealso{\code{\link{pie3D}}, \code{\link{draw.tilted.sector}}}
\examples{
 pieval<-c(2,4,6,8)
 bisectors<-pie3D(pieval,explode=0.1,main="3D PIE")
 pie3D.labels(bisectors,labels=c("TWO","FOUR","SIX","EIGHT"))
}
\keyword{misc}
