\name{thigmophobe}
\title{Find the direction away from the closest point}
\description{Find the direction away from the closest point}
\usage{
 thigmophobe(x,y)
}
\alias{thigmophobe}
\arguments{
 \item{x,y}{Numeric data vectors. Typically the x/y coordinates of
  plotted points.}
}
\details{
 \code{thigmophobe} returns the direction (as 1|2|3|4 - see pos= in 
 \code{text}) away from the nearest point to each of the points 
 described by \code{x} and \code{y}.
}
\note{
 Typically used to get the offset to automatically place labels
 on a scatterplot or similar using \code{thigmophobe.labels} to avoid 
 overlapping labels. The name means "one who fears being touched".
}
\value{
 A vector of directions away from the point nearest to each point.
}
\keyword{misc}
\author{Jim Lemon}
\seealso{\code{\link{thigmophobe.labels}}}
\examples{
 x<-rnorm(10)
 y<-rnorm(10)
 thigmophobe(x,y)
}
