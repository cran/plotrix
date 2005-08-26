\name{last.call}
\title{Find the last call containing a specified string.}
\usage{
 last.call(look.for="plot",look.back=10)
}
\alias{last.call}
\arguments{
 \item{look.for}{The string that will be matched in identifying the
  last call.}
 \item{look.back}{How far to look back in the command history.}
}
\description{
 \code{last.call} searches the command history for the last call that has
  a match to the string \code{look.for}. This is currently used to find
  plot calls, hence the default search string.}
}
\value{The last call (as a character variable) that contains the string
 \code{look.for}.}
\author{Jim Lemon}
\keyword{misc}
\seealso{\code{\link{legend.outside}}}
\examples{
 plot(rnorm(20),rnorm(20))
 if(dev.interactive()) last.call()
}
