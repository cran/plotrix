\name{thigmophobe}
\title{Find the direction away from the closest point}
\description{Find the direction away from the closest point}
\usage{
 thigmophobe(x,y)
}
\alias{thigmophobe}
\arguments{
 \item{x,y}{Numeric data vectors. Typically the x/y coordinates of
  plotted points. If arrays are passed, they will be silently coerced to
  numeric vectors.}
}
\details{
 \samp{thigmophobe} returns the direction (as 1|2|3|4 - see pos= in 
 \samp{text}) away from the nearest point to each of the points 
 described by \samp{x} and \samp{y}.
}
\note{
 \samp{thigmophobe} is typically used to get the offset to automatically
  place labels on a scatterplot or similar using \samp{thigmophobe.labels}
 to avoid overlapping labels. The name means "one who fears being touched".
}
\value{
 A vector of directions away from the point nearest to each point.
}
\keyword{misc}
\author{Jim Lemon - thanks to Gustaf Rydevik for the "names" bug fix
 and to Steve Ellison for the suggestion about arrays.}
\seealso{\link{thigmophobe.labels}}
\examples{
 x<-rnorm(10)
 y<-rnorm(10)
 thigmophobe(x,y)
}
