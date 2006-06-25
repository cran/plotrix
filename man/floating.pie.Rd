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
 \item{col}{the colors of the sectors - defaults to \code{rainbow}}
 \item{startpos}{The starting position for drawing sectors in radians.}
 \item{shadow}{Logical - whether to draw a shadow}
 \item{...}{graphical parameters passed to \code{polygon}}
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
\seealso{\code{\link{pie.labels}}, \code{\link{boxed.labels}},
 \code{\link{polygon.shadow}}}
\examples{
 plot(1:5,type="n",main="Floating Pie test",xlab="",ylab="",axes=FALSE)
 box()
 floating.pie(2,4,c(2,4,6,8),radius=0.5)
 floating.pie(3,3,c(1,4,7,8),radius=0.5)
 floating.pie(4,2,c(3,4,6,7),radius=0.5,shadow=TRUE)
}
\keyword{misc}
