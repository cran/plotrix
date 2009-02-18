\name{getIntersectList}
\alias{getIntersectList}
\title{Enter the information for a set intersection display}
\description{Enter the information for a set intersection display.}
\usage{
 getIntersectList(nelem,xnames=NULL)
}
\arguments{
 \item{nelem}{The number of sets for which the intersections will be displayed.}
 \item{xnames}{The labels for the set intersections. The function creates names
  from combinations of the first \samp{nelem} capital letters if none are 
  given.}
}
\details{
 \samp{getIntersectList} allows the user to manually enter the counts of
 set intersections rather than build this information from a matrix of data.
 It is probably most useful for producing an intersection diagram when the
 counts of the intersections are already known.
}
\value{A list of the counts of elements in the set intersections.}
\keyword{misc}
\author{Jim Lemon}
\seealso{\samp{\link{makeIntersectList}}, \samp{\link{intersectDiagram}}}
