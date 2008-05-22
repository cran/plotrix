\name{vectorField}
\alias{vectorField}
\title{Display magnitude/direction vectors}
\description{Display magnitude/direction vectors as arrows on an existing plot.}
\usage{

vectorField(u,v,xpos=NA,ypos=NA,scale=1,headspan=0.1,
 vecspec=c("lonlat","rad","deg"))
}
\arguments{
 \item{u,v}{x (longitude) and y (latitude) offsets OR magnitude and orientation
  in either radians or degrees. See details.}
 \item{xpos,ypos}{The centers of the vectors in user units.}
 \item{scale}{The proportion of each cell that the maximal vector will fill.
  See details.}
 \item{headspan}{The extent of the heads of the arrows as a proportion of
  cell size.}
 \item{vecspec}{How the vectors are described. See details}
}
\value{ nil }
\details{
 \code{vectorField} displays arrows on an existing plot. Each arrow is specified
 by a position on the plot \code{xpos,ypos} and either x/y offsets or
 orientation and magnitude. The default is x/y offsets, and the user must
 specify whether radians or degrees are used if the orientation/magnitude option
 is used. 
}
\author{Jim Lemon}
\seealso{\code{\link{plot}},\code{\link{smoothColors}}}
\keyword{misc}
