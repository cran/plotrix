\name{makeIntersectList}
\alias{makeIntersectList}
\title{Count set intersections}
\description{Create a list of set intersections from a matrix of indicators.}
\usage{
 makeIntersectList(x,xnames=NULL)
}
\arguments{
 \item{x}{A data frame or matrix where rows represent objects and columns
  attributes. A \code{1} or \code{TRUE} indicates that the object (row) has
  that attribute or is a member of that set (column).}
 \item{xnames}{Optional user-supplied names for the categories of x.}
}
\details{
 \code{makeIntersectList} reads a matrix (or data frame) containing only
 dichotomous values (either 0/1 or FALSE/TRUE). Each row represents an object
 and each column represents a set. A value of 1 or TRUE indicates that that
 object is a member of that set. The function creates a list of vectors that
 correspond to all combinations of the sets (set intersections) and inserts the
 counts of elements in each combination. If a row of the matrix is all zeros,
 it will not be counted, but the last element of the list returned contains the
 count of rows in \code{x} and thus non-members can be calculated.
 
 makeIntersectList combines the set or attribute names to form
 intersection names. For the intersection of sets A and B, the name will
 be AB and so on. These are the names that will be displayed by
 intersectDiagram. To change these, use the \code{xnames} argument.
}
\value{
 A list of the intersection counts or percentages and the total number of
 objects.
}
\keyword{misc}
\author{Jim Lemon}
\seealso{\code{\link{intersectDiagram}}, \code{\link{pasteCols}}}
\examples{
 # create a matrix where each row represents an element and
 # a 1 (or TRUE) in each column indicates that the element is a member
 # of that set.
 setdf<-data.frame(A=sample(c(0,1),100,TRUE,prob=c(0.7,0.3)),
  B=sample(c(0,1),100,TRUE,prob=c(0.7,0.3)),
  C=sample(c(0,1),100,TRUE,prob=c(0.7,0.3)),
  D=sample(c(0,1),100,TRUE,prob=c(0.7,0.3)))
 setdflist<-makeIntersectList(setdf)
 setdflist
}
