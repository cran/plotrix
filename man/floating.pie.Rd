\name{floating.pie}
\alias{floating.pie}
\title{Display a floating pie chart}
\description{
 Displays a pie chart at an arbitrary position on an existing plot
}
\usage{floating.pie(xpos,ypos,x,edges=200,radius=1,col=NULL,...)}
\arguments{
 \item{xpos,ypos}{x and y position of the center of the pie chart}
 \item{x}{a numeric vector for which each value will be a sector}
 \item{edges}{the number of lines forming a circle}
 \item{radius}{the radius of the pie in user units}
 \item{col}{the colors of the sectors - defaults to \code{rainbow}}
 \item{...}{graphical parameters passed to \code{polygon}}
}
\value{
 The bisecting angle of the sectors in radians. Useful for placing
 text labels for each sector.
}
\author{Jim Lemon}
\seealso{\code{pie.labels}, \code{boxed.labels}}
\examples{
 pieval<-c(2,4,6,8)
 plot(1:5,type="n")
 floating.pie(3,3,pieval)
}
\keyword{misc}
