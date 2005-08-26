\name{rescale}
\title{Scale a vector of numeric values into a new range.}
\usage{
 rescale(x,newrange)
}
\alias{rescale}
\arguments{
 \item{x}{A numeric vector.}
 \item{newrange}{The minimum and maximum value of the range into which
  \code{x} will be scaled.}
}
\description{
 \code{rescale} performs a simple linear conversion of \code{x} into the
  range specified by \code{newrange}. Only numeric vectors with some variation
  will be accepted.}
}
\value{On success, the rescaled vector.}
\author{Jim Lemon}
\examples{
 # scale one vector into the range of another
 normal.counts<-tabulate(cut(rnorm(100),breaks=seq(-3,3,by=1)))
 normal.density<-rescale(dnorm(seq(-3,3,length=100)),range(normal.counts))
 # now plot them
 plot(c(-2.5,-1.5,-0.5,0.5,1.5,2.5),normal.counts,type="h")
 lines(seq(-3,3,length=100),normal.density)
}
\keyword{misc}
