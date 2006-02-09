\name{integer.frequency}
\alias{integer.frequency}
\title{Calculate frequencies of a vector of integers}
\description{
 Calculates the frequencies of integers in a vector.
}
\usage{
 integer.frequency(x,bins)
}
\arguments{
  \item{x}{a vector of integers.}
  \item{bins}{The values to count in x. These values may be a subset or 
  superset of the actual values included in \code{x}.}
}
\details{
 \code{integer.frequency} counts the number of values in \code{x} that are
 equal to each of the values in \code{bins}. If there are values in \code{bins}
 that are not in \code{x}, the count for that bin will be zero, and if values in
 \code{x} are not included in \code{bins}, they will be ignored. It will not
 handle NAs correctly. However, it is only meant to be called by \code{freq}.
}
\value{A vector of counts.}
\author{Jim Lemon}
\seealso{\code{\link{freq}}}
\examples{
 testvec<-c(sample(1:10,100,TRUE),rep(12,5))
 # leave out the 10 bin and stick in an 11 bin which will be empty
 integer.frequency(testvec,c(1:9,11,12))
}
\keyword{misc}
