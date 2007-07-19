\name{dispbars}
\alias{dispbars}
\title{ Dispersion bars }
\description{
 Display bars at specified points on a plot representing measures of
 dispersion.
}
\usage{
 dispbars(x,y,ulim,llim=ulim,ddir="y",arrow.cap=0.02,arrow.gap=0.02,...)
}
\arguments{
 \item{x,y}{x and y position of the centers of the bars}
 \item{ulim,llim}{The extent of the dispersion measures.}
 \item{ddir}{The direction in which to orient the bars.}
 \item{arrow.cap}{The width of the cap at the outer end of each bar.}
 \item{arrow.gap}{The gap to leave at the inner end of each bar.}
 \item{...}{additional arguments passed to \code{arrows}.}
}
\details{
 \code{arrow.cap} and \code{arrow.gap} are both proportions of the width or
  height of the plot, depending upon whether \code{ddir} is "y" or "x"
  respectively.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{arrows}}}
\keyword{misc}
