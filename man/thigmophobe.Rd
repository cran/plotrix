\name{thigmophobe}
\title{thigmophobe}
\usage{
 thigmophobe(x,y)
}
\alias{thigmophobe}
\arguments{
 \item{x,y}{Numeric data vectors. Typically the x/y coordinates of
  plotted points.}
}
\description{
 \code{thigmophobe} returns the direction (as N|E|S|W) away from the
  nearest point to each of the points described by \code{x} and \code{y}.
}
\details{
 Typically used to get the offset to automatically place labels on
 a scatterplot or similar using \code{thigmophobe.labels} to avoid 
 overlapping labels. The name means "one who fears being touched".
}
\value{
 A vector of directions away from the point nearest to each point.
}
\keyword{misc}
\authors{Jim Lemon}
\seealso{\code{thigmophobe.labels}}
\examples{
 x<-rnorm(10)
 y<-rnorm(10)
 thigmophobe(x,y)
}
