\name{floating.pie}
\alias{floating.pie}
\title{Display a floating pie chart}
\description{
 Displays a pie chart at an arbitrary position on an existing plot
}
\usage{
 floating.pie(xpos,ypos,x,edges=200,radius=1,col=NULL,startpos=0,
  shadow=FALSE,...)
}
\arguments{
 \item{xpos,ypos}{x and y position of the center of the pie chart}
 \item{x}{a numeric vector for which each value will be a sector}
 \item{edges}{the number of lines forming a circle}
 \item{radius}{the radius of the pie in user units}
 \item{col}{the colors of the sectors - defaults to \samp{rainbow}}
 \item{startpos}{The starting position for drawing sectors in radians.}
 \item{shadow}{Logical - whether to draw a shadow}
 \item{...}{graphical parameters passed to \samp{polygon}}
}
\value{
 The bisecting angle of the sectors in radians. Useful for placing
 text labels for each sector.
}
\note{
 As with most pie charts, simplicity is essential. Trying to display a
 complicated breakdown of data rarely succeeds.
}
\author{Jim Lemon}
\seealso{\link{pie.labels}, \link{boxed.labels}, \link{polygon.shadow}}
\examples{
 plot(1:5,type="n",main="Floating Pie test",xlab="",ylab="",axes=FALSE)
 box()
 polygon(c(0,0,5.5,5.5),c(0,3,3,0),border="#44aaff",col="#44aaff")
 floating.pie(1.7,3,c(2,4,4,2,8),radius=0.5,
  col=c("#ff0000","#80ff00","#00ffff","#44bbff","#8000ff"))
 floating.pie(3.1,3,c(1,4,5,2,8),radius=0.5,
  col=c("#ff0000","#80ff00","#00ffff","#44bbff","#8000ff"))
 floating.pie(4,1.5,c(3,4,6,7),radius=0.5,
  col=c("#ff0066","#00cc88","#44bbff","#8000ff"))
 draw.circle(3.9,2.1,radius=0.04,col="white")
 draw.circle(3.9,2.1,radius=0.04,col="white")
 draw.circle(3.9,2.1,radius=0.04,col="white")
 draw.circle(4,2.3,radius=0.04,col="white")
 draw.circle(4.07,2.55,radius=0.04,col="white")
 draw.circle(4.03,2.85,radius=0.04,col="white")
 text(c(1.7,3.1,4),c(3.7,3.7,3.7),c("Pass","Pass","Fail"))
}
\keyword{misc}
