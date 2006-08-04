\name{get.segs}
\alias{get.segs}
\title{Calculate the midpoints and limits for a centipede plot}
\description{
 Calculates midpoints and limits for a list or data frame for use with
  centipede.plot.
}
\usage{
 get.segs(x,midpoint="mean",lower.limit="std.error",upper.limit="std.error")
}
\arguments{
  \item{x}{a list or data frame.}
  \item{midpoint}{The name of the function to calculate midpoints.}
  \item{lower.limit,upper.limit}{The names of the function to calculate 
   lower and upper limits.}
}
\details{
 \code{get.segs} calls the functions whose names are passed to calculate
  midpoints and limits for each list element or data frame column.
}
\value{A matrix with four rows and as many columns as were in the object
 \code{x}. The first row contains the midpoint values, the second and third
 the lower and upper limit values respectively and the fourth row the number
 of valid observations in the columns.}
\author{Jim Lemon}
\seealso{\code{\link{centipede.plot}}}
\keyword{misc}
