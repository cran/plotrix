\name{draw.tilted.sector}
\alias{draw.tilted.sector}
\title{Display a 3D pie sector}
\description{
 Displays a 3D pie sector.
}
\usage{
 draw.tilted.sector(x=0,y=0,edges=100,radius=1,height=0.3,theta=pi/6,
  start=0,end=pi*2,border=par("fg"),col=par("bg"),explode=0)
}
\arguments{
 \item{x,y}{Position of the center of the pie sector in user units}
 \item{edges}{Number of edges to draw a complete ellipse}
 \item{radius}{the radius of the pie in user units}
 \item{height}{the height of the pie in user units}
 \item{theta}{The angle of viewing in radians}
 \item{start}{Starting angle of the sector}
 \item{end}{Ending angle of the sector}
 \item{border}{The color of the sector border lines}
 \item{col}{Color of the sector}
 \item{explode}{How far to "explode" the sectors in user units}
}
\value{
 The bisector of the pie sector in radians.
}
\details{
 \code{draw.tilted.sector} displays a single 3D pie sector. It is probably
 only useful when called from \code{\link{pie3D}}.}
}
\author{Jim Lemon}
\seealso{\code{\link{pie3D}}}
\keyword{misc}
